extern crate csound;
extern crate nannou;

#[macro_use]
extern crate lazy_static;
use std::time::Duration;
use csound::*;
use nannou::prelude::*;
use nannou::ui::prelude::*;

use std::sync::mpsc::{Sender, Receiver, TryRecvError};
use std::sync::mpsc;
use std::thread;
use std::sync::{Arc, Mutex};


 /* Defining our Csound ORC code within a multiline String */
 static ORC: &str = "
 <CsoundSynthesizer>
 <CsOptions>
 -odac
 </CsOptions>
 <CsInstruments>
 sr = 44100
 ksmps = 32
 nchnls = 2
 0dbfs  = 1

 gisine   ftgen 1, 0, 16384, 10, 1
 
 turnon 2

 instr 1

 kcps = 440
 kcar = 1
 kmod = p4
 kndx line 0, p3, 20	;intensivy sidebands
 afade line 1, p3, 0

 asig foscili .5, kcps, kcar, kmod, kndx, 1
      outs asig*afade, asig*afade

 endin

 instr 2    ; every run time same values

 kbeta  betarand 100, 1, 1 
 kfreq  chnget  \"frq\"
 kamp   chnget \"amp\"
 aout   oscili kamp, kfreq+kbeta, 1    ; & listen
    outs    aout, aout
 endin

</CsInstruments>
 <CsScore>
 </CsScore>
 </CsoundSynthesizer>
 ";

struct Channel{
    sender: Sender<PlayInstr>,
    receiver: Receiver<PlayInstr>
}

enum PlayInstr{
    InstrumentData(String),
    Frequency(f32),
    Amplitude(f32),
    Exit,
}

lazy_static!{
    static ref CHANNEL:Arc<Mutex<Channel>>= {
        let (sender, receiver ): (Sender<PlayInstr>, Receiver<PlayInstr>) = mpsc::channel();
        Arc::new( Mutex::new(Channel{
            sender,
            receiver
        }))
    };
}

const NUMBER_OF_POINTS:u32 = 200;
const HALF_THICKNESS: f32 = 2.0;


fn main() {
    let event_receiver = thread::spawn(move ||{
        // Creates the csound instance
        let csound = Csound::new();
        csound.message_string_callback(|_, m: &str| print!("{}", m) );
        csound.set_option("-odac").unwrap();
        csound.compile_csd_text(ORC).unwrap();
        csound.start().unwrap();

        loop{
            csound.perform_ksmps();
            match CHANNEL.lock().unwrap().receiver.try_recv(){
                Ok(msg) => {
                    match msg {
                        PlayInstr::InstrumentData(data) => csound.send_input_message_async(&data).unwrap(),
                        PlayInstr::Frequency(value) => {
                            csound.set_control_channel("frq", value as f64);
                        },
                        
                        PlayInstr::Amplitude(amp) => {
                            csound.set_control_channel("amp", amp as f64);
                        },
                        PlayInstr::Exit => break,
                    }
                }
                
                Err(TryRecvError::Disconnected) => {
                    println!("Terminating.");
                    break;
                }
                _  => {},
            }
        
        }
        csound.stop();
    });
    
    nannou::run(model, event, view);
    let sender = CHANNEL.lock().unwrap().sender.clone();
    sender.send( PlayInstr::Exit ).unwrap();
    event_receiver.join().expect("oops! the child thread panicked");
}

struct Model {
   ui: Ui,
   ids: Ids,
   frequency: f32,
   amplitude: f32,
   color: Rgb,
   sender: Sender<PlayInstr>
}

struct Ids {
   frequency: widget::Id,
   amplitude: widget::Id,
   random_color: widget::Id,
}

fn model(app: &App) -> Model {
   
    // Set the loop mode to wait for events, an energy-efficient option for pure-GUI apps.
   app.set_loop_mode(LoopMode::Rate{ update_interval: Duration::from_millis(1) } );

   app.set_exit_on_escape(true);

   // Gets the message sender for communications to the audio engine
   let sender = CHANNEL.lock().unwrap().sender.clone();

   // Create the UI.
   let mut ui = app.new_ui().build().unwrap();

   // Generate some ids for our widgets.
   let ids = Ids {
       frequency: ui.generate_widget_id(),
       amplitude: ui.generate_widget_id(),
       random_color: ui.generate_widget_id(),
   };

   // Init our variables
   let frequency        = 0.0;
   let amplitude        = 0.0;
   let color            = Rgb::new(0.2, 0.2, 0.2);

   let model = Model {
       ui,
       ids,
       frequency,
       amplitude,
       color,
       sender
   };
   model
}

fn event(_app: &App, mut model: Model, event: Event) -> Model {

   if let Event::Update(_update) = event {
       // Calling `set_widgets` allows us to instantiate some widgets.
       let ui = &mut model.ui.set_widgets();

       fn slider(val: f32, min: f32, max: f32) -> widget::Slider<'static, f32> {
           widget::Slider::new(val, min, max)
               .w_h(200.0, 30.0)
               .label_font_size(15)
               .rgb(0.3, 0.3, 0.3)
               .label_rgb(1.0, 1.0, 1.0)
               .border(0.0)
       }

       for value in slider(model.frequency as f32, 10.0, 2000.0)
           .top_left_with_margin(20.0)
           .label("Frequency")
           .set(model.ids.frequency, ui)
       {
           model.frequency = value as f32;
           model.sender.send( PlayInstr::Frequency(value) ).unwrap();
           println!("frequency {}", value);
       }

       for value in slider(model.amplitude, 0.0, 1.0)
           .down(10.0)
           .label("Amplitude")
           .set(model.ids.amplitude, ui)
       {
           model.amplitude = value;
           model.sender.send( PlayInstr::Amplitude(value) ).unwrap();
           println!("amplitude {}", value);
       }

       for _click in widget::Button::new()
           .down(10.0)
           .w_h(200.0, 60.0)
           .label("Random sound")
           .label_font_size(15)
           .rgb(model.color.red, model.color.green, model.color.blue)
           .label_rgb(1.0, 1.0, 1.0)
           .border(0.0)
           .set(model.ids.random_color, ui)
       {
           model.color = Rgb::new(random(), random(), random());
           let sound = format!("i 1 0 2 {}", random_f64());
           model.sender.send( PlayInstr::InstrumentData(sound) ).unwrap();
       }

   }
   model
}

// Draw the state of your `Model` into the given `Frame` here.
fn view(app: &App, model: &Model, frame: Frame) -> Frame {

    // Begin drawing
   let draw = app.draw();

   draw.background().rgb(0.01, 0.01, 0.01);

   let t = app.time;
   let win = app.window_rect();

   let n_points = NUMBER_OF_POINTS;
   let half_thickness = HALF_THICKNESS;
   let hz = model.frequency;

   let vertices = (0..n_points)
       .map( |i| {
           let x = map_range(i, 0, n_points-1,win.left(), win.right());
           let fract = i as f32 / n_points as f32;
           let amp = (t + fract * hz * TAU).sin();
           let y = map_range(amp, -1.0, 1.0, win.bottom() * 0.75, win.top() * 0.75);
           pt2(x, y)
       })
       .enumerate()
       .map(|(i, p)| {
           let fract = i as f32 / n_points as f32;
           let r = (t + fract) % 1.0;
           let g = (t + 1.0 - fract) % 1.0;
           let b = (t + 0.5 + fract) % 1.0;
           let rgba = nannou::color::Rgba::new(r, g, b, 1.0);
           geom::vertex::Rgba(p, rgba)
       });

   draw.polyline().vertices(half_thickness, vertices);


   // Write the result of our drawing to the window's OpenGL frame.
   draw.to_frame(app, &frame).unwrap();

   // Draw the state of the `Ui` to the frame.
   model.ui.draw_to_frame(app, &frame).unwrap();

   // Return the drawn frame.
   frame
}
