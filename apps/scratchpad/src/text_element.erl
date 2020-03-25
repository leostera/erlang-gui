-module(text_element).

-behavior(gen_server).

-export([ draw/1
        ]).

%% What are some things a text element needs to be configured with?
%%
%% - dimensions
%% - the actual text to be displayed
%% - font configuration:
%%    - family
%%    - size
%%    - kerning
%%    - char spacing
%% - layout configuration:
%%    - line_height
%% - color
%%

draw(#{ dim := Dim={W,H}
      , text := Text
      , font_height := FontHeight
      }=State) ->
  Canvas = sk_canvas:new(W, H),

  TextPaint = sk_paint:new(),
  sk_paint:set_style(TextPaint, sk_paint:style_stroke_and_fill()),
  sk_paint:set_stroke_width(TextPaint, 0.5),
  sk_paint:set_color(TextPaint, sk_color:rgba(30, 30, 30, 255)),

  FontStyle = sk_font_style:default(),
  Typeface = sk_typeface:new(<<"Ubuntu Mono">>, FontStyle),
  Font = sk_font:new(Typeface),
  sk_font:set_size(Font, 30.0),

  LineCount = lists:foldl(
   fun (<<"">>, Offset) -> Offset+1;
       (T, Offset) ->
       TextBlob = sk_text_blob:from_binary(binary:bin_to_list(T), Font),
       sk_canvas:draw_text_blob( Canvas
                               , TextBlob
                               , 0
                               , Offset*FontHeight
                               , TextPaint),
       Offset+1
   end, 1, text:lines(Text)),

  sk_picture:from_canvas(Canvas).
