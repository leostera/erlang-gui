#[macro_use]
extern crate rustler;

use std::io::Write;

use std::sync::RwLock;

use rustler::types::OwnedBinary;
use rustler::{Encoder, Env, Error, ResourceArc, Term};

use skia_safe::{
    Color, Data, Font, FontStyle, Paint, PaintStyle, Path, Picture, PictureRecorder, RRect, Rect,
    TextBlob, TextEncoding, Typeface,
};

struct CanvasResource {
    data: RwLock<PictureRecorder>,
}

impl CanvasResource {
    pub fn as_picture(&self) -> Picture {
        self.data
            .write()
            .unwrap()
            .finish_recording_as_picture(None)
            .unwrap()
    }
}

struct ColorResource {
    data: RwLock<Color>,
}

struct FontResource {
    data: RwLock<Font>,
}

struct FontStyleResource {
    data: RwLock<FontStyle>,
}

struct PaintResource {
    data: RwLock<Paint>,
}

struct PictureResource {
    data: RwLock<Picture>,
}

impl PictureResource {
    pub fn serialize(&self) -> Data {
        self.data.write().unwrap().serialize()
    }
}

struct PathResource {
    data: RwLock<Path>,
}

struct RectResource {
    data: RwLock<Rect>,
}

struct RRectResource {
    data: RwLock<RRect>,
}

struct PaintStyleResource {
    data: RwLock<PaintStyle>,
}

struct TextBlobResource {
    data: RwLock<TextBlob>,
}

struct TypefaceResource {
    data: RwLock<Typeface>,
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler::rustler_export_nifs! {
    "skia_native",
    [
        ("sk_canvas__clear", 2, sk_canvas__clear),
        ("sk_canvas__clip_rect", 3, sk_canvas__clip_rect),
        ("sk_canvas__draw_circle", 5, sk_canvas__draw_circle),
        ("sk_canvas__draw_color", 2, sk_canvas__draw_color),
        ("sk_canvas__draw_paint", 2, sk_canvas__draw_paint),
        ("sk_canvas__draw_path", 3, sk_canvas__draw_path),
        ("sk_canvas__draw_picture", 2, sk_canvas__draw_picture),
        ("sk_canvas__draw_pictures", 2, sk_canvas__draw_pictures),
        ("sk_canvas__draw_rect", 3, sk_canvas__draw_rect),
        ("sk_canvas__draw_round_rect", 5, sk_canvas__draw_round_rect),
        ("sk_canvas__draw_rrect", 3, sk_canvas__draw_rrect),
        ("sk_canvas__draw_text_blob", 5, sk_canvas__draw_text_blob),
        ("sk_canvas__new", 2, sk_canvas__new),
        ("sk_canvas__translate", 3, sk_canvas__translate),
        ("sk_color__blue", 0, sk_color__blue),
        ("sk_color__cyan", 0, sk_color__cyan),
        ("sk_color__green", 0, sk_color__green),
        ("sk_color__red", 0, sk_color__red),
        ("sk_color__rgba", 4, sk_color__rgba),
        ("sk_color__yellow", 0, sk_color__yellow),
        ("sk_font__new", 1, sk_font__new),
        ("sk_font__default", 0, sk_font__default),
        ("sk_font__set_size", 2, sk_font__set_size),
        ("sk_font_style__default",0, sk_font_style__default),
        ("sk_font_style__new",3, sk_font_style__new),
        ("sk_font_style__slant",1, sk_font_style__slant),
        ("sk_font_style__weight",1, sk_font_style__weight),
        ("sk_font_style__width",1, sk_font_style__width),
        ("sk_paint__new", 0, sk_paint__new),
        ("sk_paint__set_color", 2, sk_paint__set_color),
        ("sk_paint__set_stroke_width", 2, sk_paint__set_stroke_width),
        ("sk_paint__set_style", 2, sk_paint__set_style),
        ("sk_paint__style_fill",0,sk_paint__style_fill),
        ("sk_paint__style_stroke",0,sk_paint__style_stroke),
        ("sk_paint__style_stroke_and_fill",0,sk_paint__style_stroke_and_fill),
        ("sk_path__close", 1, sk_path__close),
        ("sk_path__cubic", 7, sk_path__cubic),
        ("sk_path__line_to", 3, sk_path__line_to),
        ("sk_path__move_to", 3, sk_path__move_to),
        ("sk_path__new", 0, sk_path__new),
        ("sk_picture__as_bytes", 1, sk_picture__as_bytes),
        ("sk_picture__from_canvas", 1, sk_picture__from_canvas),
        ("sk_rect__make_xywh", 4, sk_rect__make_xywh),
        ("sk_rrect__new", 0, sk_rrect__new),
        ("sk_rrect__offset", 3, sk_rrect__offset),
        ("sk_rrect__set_oval", 2, sk_rrect__set_oval),
        ("sk_text_blob__from_binary", 2, sk_text_blob__from_binary),
        ("sk_typeface__default", 0, sk_typeface__default),
        ("sk_typeface__new", 2, sk_typeface__new),
    ],
    Some(on_init)
}

fn on_init<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    resource_struct_init!(CanvasResource, env);
    resource_struct_init!(ColorResource, env);
    resource_struct_init!(FontResource, env);
    resource_struct_init!(FontStyleResource, env);
    resource_struct_init!(PaintResource, env);
    resource_struct_init!(PaintStyleResource, env);
    resource_struct_init!(PathResource, env);
    resource_struct_init!(PictureResource, env);
    resource_struct_init!(RRectResource, env);
    resource_struct_init!(RectResource, env);
    resource_struct_init!(TextBlobResource, env);
    resource_struct_init!(TypefaceResource, env);
    true
}

fn sk_canvas__draw_circle<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let radius: f32 = args[1].decode()?;
    let x: f32 = args[2].decode()?;
    let y: f32 = args[3].decode()?;
    let paint_resource: ResourceArc<PaintResource> = args[4].decode()?;

    let paint = paint_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_circle((x, y), radius, &paint);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_color<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let color_resource: ResourceArc<ColorResource> = args[1].decode()?;

    let color = color_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_color(color.clone(), None);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_paint<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let paint_resource: ResourceArc<PaintResource> = args[1].decode()?;

    let paint = paint_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_paint(&paint);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_path<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let path_resource: ResourceArc<PathResource> = args[1].decode()?;
    let paint_resource: ResourceArc<PaintResource> = args[2].decode()?;

    let path = path_resource.data.write().unwrap();
    let paint = paint_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_path(&path, &paint);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_picture<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let picture_resource: ResourceArc<PictureResource> = args[1].decode()?;

    let picture = picture_resource.data.write().unwrap().clone();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_picture(picture, None, None);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_pictures<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let pictures_vector: Vec<((f32, f32, f32, i64), ResourceArc<PictureResource>)> =
        args[1].decode()?;

    let canvas = &mut canvas_resource.data.write().unwrap();

    for ((_z, y, x, _ref), pic_res) in pictures_vector {
        let picture = &*pic_res.data.read().unwrap();
        canvas
            .recording_canvas()
            .translate((x, y))
            .draw_picture(picture, None, None)
            .translate((-x, -y));
    }

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_rect<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let rect_resource: ResourceArc<RectResource> = args[1].decode()?;
    let paint_resource: ResourceArc<PaintResource> = args[2].decode()?;

    let rect = rect_resource.data.write().unwrap();
    let paint = paint_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_rect(*rect, &paint);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_round_rect<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let rect_resource: ResourceArc<RectResource> = args[1].decode()?;
    let x: f32 = args[2].decode()?;
    let y: f32 = args[3].decode()?;
    let paint_resource: ResourceArc<PaintResource> = args[4].decode()?;

    let rect = rect_resource.data.write().unwrap();
    let paint = paint_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_round_rect(*rect, x, y, &paint);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_rrect<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let rrect_resource: ResourceArc<RRectResource> = args[1].decode()?;
    let paint_resource: ResourceArc<PaintResource> = args[2].decode()?;

    let rrect = rrect_resource.data.write().unwrap().clone();
    let paint = paint_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_rrect(rrect, &paint);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__clear<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let color_resource: ResourceArc<ColorResource> = args[1].decode()?;

    let color = color_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .clear(color.clone());

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__translate<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let x: f32 = args[1].decode()?;
    let y: f32 = args[2].decode()?;

    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .translate((x, y));

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__clip_rect<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let w: i32 = args[1].decode()?;
    let h: i32 = args[2].decode()?;

    let rect = Rect::from_iwh(w, h);
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .clip_rect(rect, None, true);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__draw_text_blob<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas_resource: ResourceArc<CanvasResource> = args[0].decode()?;
    let text_blob_resource: ResourceArc<TextBlobResource> = args[1].decode()?;
    let x: i32 = args[2].decode()?;
    let y: i32 = args[3].decode()?;
    let paint_resource: ResourceArc<PaintResource> = args[4].decode()?;

    let text = text_blob_resource.data.write().unwrap().clone();
    let paint = paint_resource.data.write().unwrap();
    canvas_resource
        .data
        .write()
        .unwrap()
        .recording_canvas()
        .draw_text_blob(&text, (x, y), &paint);

    Ok(canvas_resource.encode(env))
}

fn sk_canvas__new<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let width: i32 = args[0].decode()?;
    let height: i32 = args[1].decode()?;
    let mut recorder = PictureRecorder::new();
    recorder.begin_recording(Rect::from_iwh(width, height), None, None);
    let canvas_resource = CanvasResource {
        data: RwLock::new(recorder),
    };
    Ok(ResourceArc::new(canvas_resource).encode(env))
}

/*
pub const TRANSPARENT: Self = Self(sb::SK_ColorTRANSPARENT);
pub const BLACK: Self = Self(sb::SK_ColorBLACK);
pub const DARK_GRAY: Self = Self(sb::SK_ColorDKGRAY);
pub const GRAY: Self = Self(sb::SK_ColorLTGRAY);
pub const LIGHT_GRAY: Self = Self(sb::SK_ColorLTGRAY);
pub const WHITE: Self = Self(sb::SK_ColorWHITE);
pub const RED: Self = Self(sb::SK_ColorRED);
pub const GREEN: Self = Self(sb::SK_ColorGREEN);
pub const BLUE: Self = Self(sb::SK_ColorBLUE);
pub const YELLOW: Self = Self(sb::SK_ColorYELLOW);
pub const CYAN: Self = Self(sb::SK_ColorCYAN);
pub const MAGENTA: Self = Self(sb::SK_ColorMAGENTA);
*/

fn sk_color__blue<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(ResourceArc::new(ColorResource {
        data: RwLock::new(Color::BLUE),
    })
    .encode(env))
}

fn sk_color__cyan<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(ResourceArc::new(ColorResource {
        data: RwLock::new(Color::CYAN),
    })
    .encode(env))
}

fn sk_color__green<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(ResourceArc::new(ColorResource {
        data: RwLock::new(Color::GREEN),
    })
    .encode(env))
}

fn sk_color__red<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(ResourceArc::new(ColorResource {
        data: RwLock::new(Color::RED),
    })
    .encode(env))
}

fn sk_color__yellow<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(ResourceArc::new(ColorResource {
        data: RwLock::new(Color::YELLOW),
    })
    .encode(env))
}

fn sk_color__rgba<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let r: u8 = args[0].decode()?;
    let g: u8 = args[1].decode()?;
    let b: u8 = args[2].decode()?;
    let a: u8 = args[3].decode()?;

    Ok(ResourceArc::new(ColorResource {
        data: RwLock::new(Color::from_argb(a, r, g, b)),
    })
    .encode(env))
}

fn sk_font__default<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let font = Font::default();
    let font_resource = FontResource {
        data: RwLock::new(font),
    };
    Ok(ResourceArc::new(font_resource).encode(env))
}

fn sk_font__new<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let typeface_resource: ResourceArc<TypefaceResource> = args[0].decode()?;

    let typeface = &*typeface_resource.data.read().unwrap();
    let font_resource = FontResource {
        data: RwLock::new(Font::from_typeface(typeface, None)),
    };
    Ok(ResourceArc::new(font_resource).encode(env))
}

fn sk_font__set_size<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let font_resource: ResourceArc<FontResource> = args[0].decode()?;
    let size: f32 = args[1].decode()?;
    font_resource.data.write().unwrap().set_size(size);
    Ok(font_resource.encode(env))
}

fn sk_paint__new<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let paint_resource = PaintResource {
        data: RwLock::new(Paint::default()),
    };
    Ok(ResourceArc::new(paint_resource).encode(env))
}

fn sk_paint__set_color<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let paint_resource: ResourceArc<PaintResource> = args[0].decode()?;
    let color_resource: ResourceArc<ColorResource> = args[1].decode()?;

    let color = color_resource.data.write().unwrap();
    let mut paint = paint_resource.data.write().unwrap();
    paint.set_color(color.clone());

    Ok(paint_resource.encode(env))
}

fn sk_paint__set_stroke_width<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let paint_resource: ResourceArc<PaintResource> = args[0].decode()?;
    let stroke_width: f32 = args[1].decode()?;

    let mut paint = paint_resource.data.write().unwrap();
    paint.set_stroke_width(stroke_width);

    Ok(paint_resource.encode(env))
}

fn sk_paint__style_fill<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(ResourceArc::new(PaintStyleResource {
        data: RwLock::new(PaintStyle::Fill),
    })
    .encode(env))
}

fn sk_paint__style_stroke_and_fill<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(ResourceArc::new(PaintStyleResource {
        data: RwLock::new(PaintStyle::StrokeAndFill),
    })
    .encode(env))
}

fn sk_paint__style_stroke<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(ResourceArc::new(PaintStyleResource {
        data: RwLock::new(PaintStyle::Stroke),
    })
    .encode(env))
}

fn sk_paint__set_style<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let paint_resource: ResourceArc<PaintResource> = args[0].decode()?;
    let style_resource: ResourceArc<PaintStyleResource> = args[1].decode()?;

    let style = style_resource.data.write().unwrap();
    paint_resource.data.write().unwrap().set_style(*style);

    Ok(paint_resource.encode(env))
}

fn sk_path__close<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let path_resource: ResourceArc<PathResource> = args[0].decode()?;

    path_resource.data.write().unwrap().close();

    Ok(path_resource.encode(env))
}

fn sk_path__line_to<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let path_resource: ResourceArc<PathResource> = args[0].decode()?;
    let x0: f32 = args[1].decode()?;
    let y0: f32 = args[2].decode()?;

    path_resource.data.write().unwrap().line_to((x0, y0));

    Ok(path_resource.encode(env))
}

fn sk_path__move_to<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let path_resource: ResourceArc<PathResource> = args[0].decode()?;
    let x0: f32 = args[1].decode()?;
    let y0: f32 = args[2].decode()?;

    path_resource.data.write().unwrap().move_to((x0, y0));

    Ok(path_resource.encode(env))
}

fn sk_path__cubic<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let path_resource: ResourceArc<PathResource> = args[0].decode()?;
    let x0: i32 = args[1].decode()?;
    let y0: i32 = args[2].decode()?;
    let x1: i32 = args[3].decode()?;
    let y1: i32 = args[4].decode()?;
    let x2: i32 = args[5].decode()?;
    let y2: i32 = args[6].decode()?;

    path_resource
        .data
        .write()
        .unwrap()
        .cubic_to((x0, y0), (x1, y1), (x2, y2));

    Ok(path_resource.encode(env))
}

fn sk_path__new<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let path_resource = PathResource {
        data: RwLock::new(Path::new()),
    };
    Ok(ResourceArc::new(path_resource).encode(env))
}

fn sk_picture__as_bytes<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let picture_resource: ResourceArc<PictureResource> = args[0].decode()?;

    let bytes: Vec<u8> = picture_resource.serialize().as_bytes().to_vec();

    let mut binary = OwnedBinary::new(bytes.len()).unwrap();
    let _ = binary.as_mut_slice().write_all(&bytes);

    Ok(binary.release(env).encode(env))
}

fn sk_picture__from_canvas<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let canvas: ResourceArc<CanvasResource> = args[0].decode()?;
    let picture = canvas.as_picture();
    let picture_resource = PictureResource {
        data: RwLock::new(picture),
    };
    Ok(ResourceArc::new(picture_resource).encode(env))
}

fn sk_rect__make_xywh<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let x: i32 = args[0].decode()?;
    let y: i32 = args[1].decode()?;
    let w: i32 = args[2].decode()?;
    let h: i32 = args[3].decode()?;

    let rect_resource = RectResource {
        data: RwLock::new(Rect::from_xywh(x as f32, y as f32, w as f32, h as f32)),
    };

    Ok(ResourceArc::new(rect_resource).encode(env))
}

fn sk_rrect__new<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let rrect_resource = RRectResource {
        data: RwLock::new(RRect::new()),
    };

    Ok(ResourceArc::new(rrect_resource).encode(env))
}

fn sk_rrect__offset<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let rrect_resource: ResourceArc<RRectResource> = args[0].decode()?;
    let x: i32 = args[1].decode()?;
    let y: i32 = args[2].decode()?;

    let mut rrect = rrect_resource.data.write().unwrap();
    rrect.offset((x, y));

    Ok(rrect_resource.encode(env))
}

fn sk_rrect__set_oval<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let rrect_resource: ResourceArc<RRectResource> = args[0].decode()?;
    let rect_resource: ResourceArc<RectResource> = args[1].decode()?;

    let rect = rect_resource.data.write().unwrap();
    let mut rrect = rrect_resource.data.write().unwrap();
    rrect.set_oval(rect.clone());

    Ok(rrect_resource.encode(env))
}

fn sk_text_blob__from_binary<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let text: Vec<u8> = args[0].decode()?;
    let font_resource: ResourceArc<FontResource> = args[1].decode()?;

    let font = font_resource.data.write().unwrap();
    let text_blob_resource = TextBlobResource {
        data: RwLock::new(TextBlob::from_text(text.as_slice(), TextEncoding::UTF8, &font).unwrap()),
    };

    Ok(ResourceArc::new(text_blob_resource).encode(env))
}

fn sk_typeface__default<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let typeface = TypefaceResource {
        data: RwLock::new(Typeface::default()),
    };
    Ok(ResourceArc::new(typeface).encode(env))
}

fn sk_typeface__new<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let name: String = args[0].decode()?;
    let font_style: ResourceArc<FontStyleResource> = args[1].decode()?;

    let typeface = TypefaceResource {
        data: RwLock::new(
            match Typeface::new(&name, *font_style.data.read().unwrap()) {
                Some(t) => t,
                None => Typeface::default(),
            },
        ),
    };
    Ok(ResourceArc::new(typeface).encode(env))
}

fn sk_font_style__default<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let font_style = FontStyle::default();
    let font_style_resource = FontStyleResource {
        data: RwLock::new(font_style),
    };
    Ok(ResourceArc::new(font_style_resource).encode(env))
}

fn sk_font_style__new<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let weight: i32 = args[0].decode()?;
    let width: i32 = args[1].decode()?;
    let slant: i32 = args[2].decode()?;

    let font_style = FontStyle::new(
        skia_safe::font_style::Weight::from(weight),
        skia_safe::font_style::Width::from(width),
        match slant {
            1 => skia_safe::font_style::Slant::Italic,
            2 => skia_safe::font_style::Slant::Oblique,
            _ => skia_safe::font_style::Slant::Upright,
        },
    );
    let font_style_resource = FontStyleResource {
        data: RwLock::new(font_style),
    };
    Ok(ResourceArc::new(font_style_resource).encode(env))
}

fn sk_font_style__slant<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let font_style_resource: ResourceArc<FontStyleResource> = args[0].decode()?;
    let slant: i32 = font_style_resource.data.write().unwrap().slant() as i32;
    Ok(slant.encode(env))
}

fn sk_font_style__weight<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let font_style_resource: ResourceArc<FontStyleResource> = args[0].decode()?;
    let weight: i32 = *font_style_resource.data.write().unwrap().weight();
    Ok(weight.encode(env))
}

fn sk_font_style__width<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let font_style_resource: ResourceArc<FontStyleResource> = args[0].decode()?;
    let width: i32 = *font_style_resource.data.write().unwrap().width();
    Ok(width.encode(env))
}
