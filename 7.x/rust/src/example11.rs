/* Example 11 - A Nannou user interface
 *
 * This is the Csound 7 / csound-rs 0.3 port of the original Csound 6 UI
 * example. It uses Nannou 0.20's egui integration and runs Csound on its own
 * thread.
 */

use csound::{Csound, Dispatch};
use nannou::prelude::egui;
use nannou::prelude::*;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender, TryRecvError};
use std::sync::{Arc, Mutex, OnceLock};
use std::thread;

/* Defining our Csound ORC code within a multiline String */
static ORC: &str = "
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
 ";

struct Channel {
    sender: Sender<PlayInstr>,
    receiver: Receiver<PlayInstr>,
}

enum PlayInstr {
    InstrumentData(String),
    Frequency(f32),
    Amplitude(f32),
    Exit,
}

static CHANNEL: OnceLock<Arc<Mutex<Channel>>> = OnceLock::new();

fn channel() -> &'static Arc<Mutex<Channel>> {
    CHANNEL.get_or_init(|| {
        let (sender, receiver): (Sender<PlayInstr>, Receiver<PlayInstr>) = mpsc::channel();
        Arc::new(Mutex::new(Channel { sender, receiver }))
    })
}

const NUMBER_OF_POINTS: u32 = 200;
const HALF_THICKNESS: f32 = 2.0;

fn main() {
    let event_receiver = thread::spawn(move || {
        // Create the Csound instance on the audio thread.
        let csound = Csound::new().expect("failed to create a Csound instance");
        csound.set_option("-odac").unwrap();
        csound.compile_orc(ORC, Dispatch::Sync).unwrap();
        csound.start().unwrap();

        loop {
            csound.perform_ksmps();
            match channel().lock().unwrap().receiver.try_recv() {
                Ok(msg) => match msg {
                    PlayInstr::InstrumentData(data) => {
                        csound.send_string_event(&data, Dispatch::Async).unwrap()
                    }
                    PlayInstr::Frequency(value) => {
                        csound.set_control_channel("frq", value as f64).unwrap();
                    }
                    PlayInstr::Amplitude(amp) => {
                        csound.set_control_channel("amp", amp as f64).unwrap();
                    }
                    PlayInstr::Exit => break,
                },
                Err(TryRecvError::Disconnected) => {
                    println!("Terminating.");
                    break;
                }
                Err(TryRecvError::Empty) => {}
            }
        }
        // Csound 0.3 cleans up when the Csound value is dropped.
    });

    nannou::app(model).update(update).simple_window(view).run();

    let sender = channel().lock().unwrap().sender.clone();
    sender.send(PlayInstr::Exit).unwrap();
    event_receiver
        .join()
        .expect("oops! the child thread panicked");
}

struct Model {
    frequency: f32,
    amplitude: f32,
    color: egui::Color32,
    sender: Sender<PlayInstr>,
}

fn model(_app: &App) -> Model {
    // Get the sender used to communicate with the audio engine.
    let sender = channel().lock().unwrap().sender.clone();

    Model {
        frequency: 0.0,
        amplitude: 0.0,
        color: egui::Color32::from_rgb(51, 51, 51),
        sender,
    }
}

fn update(app: &App, model: &mut Model) {
    let context = app.egui();

    egui::Window::new("Csound controls").show(&context, |ui| {
        if ui
            .add(egui::Slider::new(&mut model.frequency, 10.0..=2000.0).text("Frequency"))
            .changed()
        {
            model
                .sender
                .send(PlayInstr::Frequency(model.frequency))
                .unwrap();
            println!("frequency {}", model.frequency);
        }

        if ui
            .add(egui::Slider::new(&mut model.amplitude, 0.0..=1.0).text("Amplitude"))
            .changed()
        {
            model
                .sender
                .send(PlayInstr::Amplitude(model.amplitude))
                .unwrap();
            println!("amplitude {}", model.amplitude);
        }

        if ui
            .add(egui::Button::new("Random sound").fill(model.color))
            .clicked()
        {
            model.color = egui::Color32::from_rgb(rand::random(), rand::random(), rand::random());
            let sound = format!("i 1 0 2 {}", rand::random::<f64>());
            model.sender.send(PlayInstr::InstrumentData(sound)).unwrap();
        }
    });
}

fn view(app: &App, model: &Model, _window: Entity) {
    let draw = app.draw();

    draw.background().srgb(0.01, 0.01, 0.01);

    let t = app.time();
    let win = app.window_rect();

    let vertices = (0..NUMBER_OF_POINTS)
        .map(|i| {
            let x = map_range(i, 0, NUMBER_OF_POINTS - 1, win.left(), win.right());
            let fract = i as f32 / NUMBER_OF_POINTS as f32;
            let amp = (t + fract * model.frequency * TAU).sin();
            let y = map_range(amp, -1.0, 1.0, win.bottom() * 0.75, win.top() * 0.75);
            pt2(x, y)
        })
        .enumerate()
        .map(|(i, p)| {
            let fract = i as f32 / NUMBER_OF_POINTS as f32;
            let r = (t + fract) % 1.0;
            let g = (t + 1.0 - fract) % 1.0;
            let b = (t + 0.5 + fract) % 1.0;
            (p, Color::srgba(r, g, b, 1.0))
        });

    draw.polyline()
        .weight(HALF_THICKNESS)
        .points_colored(vertices);
}
