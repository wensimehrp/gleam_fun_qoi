import argv
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import pngleam
import simplifile

pub fn main() -> Nil {
  let args = argv.load()
  case args.arguments {
    [filename] -> {
      let assert Ok(data) = simplifile.read_bits(from: filename)
      let assert Ok(#(width, height, _channels, _colorspace, buffer)) =
        decode_qoi(data)
      let assert Ok(#(_, png)) =
        pngleam.render_transparent_colour_png(
          width:,
          height:,
          bit_depth: 8,
          state: buffer,
          pixel: fn(remaining, _, _) {
            case remaining {
              <<r:size(8), g:size(8), b:size(8), a:size(8), rest:bytes>> -> {
                #(rest, pngleam.Rgba(r, g, b, a))
              }
              _ -> panic
            }
          },
          compression_level: None,
        )
      let assert Ok(_) = simplifile.write_bits("sample.png", png)
      Nil
    }
    _ -> io.println_error("Usage: " <> args.program <> " filename")
  }
}

type Pixel {
  Pixel(r: Int, g: Int, b: Int, a: Int)
}

fn get_pixel_index(px: Pixel) -> Int {
  { px.r * 3 + px.g * 5 + px.b * 7 + px.a * 11 } % 64
}

fn decode_qoi(data: BitArray) -> Result(#(Int, Int, Int, Int, BitArray), Nil) {
  let first_px = <<
    0,
    0,
    0,
    255,
  >>
  case data {
    <<
      "qoif":utf8,
      width:unsigned-big-size(32),
      height:unsigned-big-size(32),
      channels:unsigned-big-size(8),
      colorspace:unsigned-big-size(8),
      data:bytes,
    >>
      if { channels == 3 || channels == 4 }
      && { colorspace == 0 || colorspace == 1 }
    -> {
      decode_qoi_payload(data, Pixel(0, 0, 0, 0) |> list.repeat(64), first_px)
      |> result.map(fn(data) { #(width, height, channels, colorspace, data) })
    }
    _ -> Error(Nil)
  }
}

fn wrapping_add_u8(a: Int, b: Int) -> Int {
  int.bitwise_and(a + b, 0xff)
}

fn normalize_delta_value(a: Int) -> Int {
  case a {
    0 -> -2
    1 -> -1
    2 -> 0
    3 -> 1
    _ -> panic
  }
}

fn update_lookup_window(
  prev_lookup: List(Pixel),
  r: Int,
  g: Int,
  b: Int,
  a: Int,
) -> List(Pixel) {
  let new_px = Pixel(r, g, b, a)
  let write_index = new_px |> get_pixel_index
  prev_lookup
  |> list.index_map(fn(x, i) {
    case i == write_index {
      True -> new_px
      False -> x
    }
  })
}

fn decode_qoi_payload(
  data: BitArray,
  lookup: List(Pixel),
  acc: BitArray,
) -> Result(BitArray, Nil) {
  case data {
    // end of stream
    <<0:size(56), 1:size(8), _rest:bytes>> -> {
      case acc {
        <<_:size(32), rest:bytes>> -> Ok(rest)
        _ -> Error(Nil)
      }
    }
    // QOI_OP_RGB
    <<
      0b11111110,
      red:unsigned-size(8),
      green:unsigned-size(8),
      blue:unsigned-size(8),
      rest:bytes,
    >> -> {
      let assert Ok(<<
        _prev_r:unsigned-size(8),
        _prev_g:unsigned-size(8),
        _prev_b:unsigned-size(8),
        prev_a:unsigned-size(8),
      >>) = acc |> bit_array.slice(at: acc |> bit_array.byte_size, take: -4)
      decode_qoi_payload(
        rest,
        lookup |> update_lookup_window(red, green, blue, prev_a),
        <<
          acc:bits,
          red:size(8),
          green:size(8),
          blue:size(8),
          prev_a:size(8),
        >>,
      )
    }
    // QOI_OP_RGBA
    <<
      0b11111111,
      red:unsigned-size(8),
      green:unsigned-size(8),
      blue:unsigned-size(8),
      alpha:unsigned-size(8),
      rest:bytes,
    >> -> {
      decode_qoi_payload(
        rest,
        lookup |> update_lookup_window(red, green, blue, alpha),
        <<
          acc:bits,
          red:size(8),
          green:size(8),
          blue:size(8),
          alpha:size(8),
        >>,
      )
    }
    // QOI_OP_INDEX
    <<0b00:size(2), index:size(6), rest:bytes>> -> {
      let assert Some(px) =
        lookup
        |> list.index_fold(None, fn(acc, px, i) {
          case acc {
            Some(found_px) -> Some(found_px)
            None ->
              case i == index {
                True -> Some(px)
                False -> None
              }
          }
        })
      decode_qoi_payload(rest, lookup, <<
        acc:bits,
        px.r:size(8),
        px.g:size(8),
        px.b:size(8),
        px.a:size(8),
      >>)
    }
    // QOI_OP_DIFF
    <<
      0b01:size(2),
      delta_r:unsigned-size(2),
      delta_g:unsigned-size(2),
      delta_b:unsigned-size(2),
      rest:bytes,
    >> -> {
      let assert Ok(<<
        prev_r:unsigned-size(8),
        prev_g:unsigned-size(8),
        prev_b:unsigned-size(8),
        prev_a:unsigned-size(8),
      >>) = acc |> bit_array.slice(at: acc |> bit_array.byte_size, take: -4)
      let curr_r = wrapping_add_u8(prev_r, delta_r |> normalize_delta_value)
      let curr_g = wrapping_add_u8(prev_g, delta_g |> normalize_delta_value)
      let curr_b = wrapping_add_u8(prev_b, delta_b |> normalize_delta_value)
      decode_qoi_payload(
        rest,
        lookup
          |> update_lookup_window(curr_r, curr_g, curr_b, prev_a),
        <<
          acc:bits,
          curr_r:size(8),
          curr_g:size(8),
          curr_b:size(8),
          prev_a:size(8),
        >>,
      )
    }
    // QOI_OP_LUMA
    <<
      0b10:size(2),
      diff_green:unsigned-size(6),
      dr_dg:unsigned-size(4),
      db_dg:unsigned-size(4),
      rest:bytes,
    >> -> {
      let assert Ok(<<
        prev_r:unsigned-size(8),
        prev_g:unsigned-size(8),
        prev_b:unsigned-size(8),
        prev_a:unsigned-size(8),
      >>) = acc |> bit_array.slice(at: acc |> bit_array.byte_size, take: -4)
      let dg = diff_green - 32
      let curr_g = wrapping_add_u8(prev_g, dg)
      let curr_r = wrapping_add_u8(prev_r, dg + dr_dg - 8)
      let curr_b = wrapping_add_u8(prev_b, dg + db_dg - 8)
      decode_qoi_payload(
        rest,
        lookup |> update_lookup_window(curr_r, curr_g, curr_b, prev_a),
        <<
          acc:bits,
          curr_r:size(8),
          curr_g:size(8),
          curr_b:size(8),
          prev_a:size(8),
        >>,
      )
    }
    // QOI_OP_RUN
    <<0b11:size(2), run_length:unsigned-size(6), rest:bytes>> -> {
      let assert Ok(prev_px) =
        acc |> bit_array.slice(at: acc |> bit_array.byte_size, take: -4)
      decode_qoi_payload(rest, lookup, <<
        acc:bits,
        { prev_px |> list.repeat(run_length + 1) |> bit_array.concat }:bits,
      >>)
    }
    _ -> Error(Nil)
  }
}
