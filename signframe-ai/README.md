
# SignFrame AI

**Portfolio package finalized:** 15 November 2026. **GitHub portfolio maintainer:** Jean Aimé Iraguha ([Aime2k2](https://github.com/Aime2k2)). **Original project author:** Tayyaba Ilyas ([original source](https://github.com/tayyabastack/signframe-ai)).

### Real-Time ASL Fingerspelling Recognition with Deep Learning

SignFrame AI is a computer vision application that recognizes American Sign Language (ASL) fingerspelling letters through a webcam and converts predictions into text and speech.

Built with **PyTorch, MediaPipe, and OpenCV**, the application combines a custom CNN, real-time hand tracking, prediction stabilization, and an interactive communication interface.

The project focuses on real-world gesture recognition, model adaptation, and human-computer interaction.

## Features

- Real-time recognition of 24 static ASL letters (excluding J and Z).
- Hand detection using MediaPipe's 21 hand landmarks.
- Custom PyTorch CNN for fingerspelling classification.
- Live predictions with confidence scores.
- Prediction stabilization before letter confirmation.
- Text composition and message editing.
- Text-to-speech for composed messages.
- Full-screen-style communication display.
- Local inference without cloud APIs.

## Tech Stack

| Technology | Purpose |
|---|---|
| Python | Application development |
| PyTorch | CNN training and inference |
| OpenCV | Webcam capture and interface |
| MediaPipe | Hand tracking and localization |
| NumPy | Image preprocessing |
| scikit-learn | Model evaluation |
| pandas | Dataset processing |
| Matplotlib | Evaluation visualizations |

Developed and tested on macOS with Apple Silicon (MPS).

## How It Works

The application uses MediaPipe to locate the hand in each webcam frame. The detected region is cropped, converted to grayscale, and resized to match the CNN input.

The CNN predicts one of 24 fingerspelling classes. Predictions are stabilized across frames before users confirm letters to build messages.

### Recognition Pipeline

```text
Webcam
   |
   v
MediaPipe Hand Detection
   |
   v
Hand Cropping & Preprocessing
   |
   v
PyTorch CNN (24 Classes)
   |
   v
Prediction Stabilization
   |
   v
Text Composition
   |
   +-- Message Editing
   +-- Text-to-Speech
   +-- Communication Display
```

MediaPipe landmarks are used for hand localization and visualization. The CNN classifies grayscale hand images rather than landmark coordinates.

## Model Architecture

The recognition model is a custom Convolutional Neural Network implemented in PyTorch.

| Layer | Configuration |
|---|---|
| Input | 28 × 28 grayscale |
| Conv2D | 32 filters, 3×3, ReLU |
| MaxPooling | 2×2 |
| Conv2D | 64 filters, 3×3, ReLU |
| MaxPooling | 2×2 |
| Flatten | 3,136 features |
| Dense | 128 neurons, ReLU |
| Dropout | 0.3 |
| Output | 24 classes |

Softmax is applied during inference to obtain class probabilities.

The application loads the feedback-adapted checkpoint from `models/feedback_candidate.pth`.

## Dataset & Training

The baseline CNN was trained on [Sign Language MNIST](https://www.kaggle.com/datasets/datamunge/sign-language-mnist).

| Dataset | Images |
|---|---:|
| Training | 27,455 |
| Testing | 7,172 |
| Image size | 28 × 28 |
| Classes | 24 |

Letters J and Z are excluded because they require movement.

### Model Adaptation

Although the baseline model performed well on Sign Language MNIST, initial webcam testing showed poor generalization to real-world inputs.

A human-in-the-loop feedback workflow was developed to collect labeled webcam examples through confirmed and corrected predictions.

The feedback dataset eventually contained **611 labeled images** across the 24 supported letters.

These examples were used to adapt the CNN to webcam inputs. The final application uses a pretrained checkpoint; feedback collection and retraining controls are not exposed to users.

## Model Evaluation

| Model / Evaluation | Accuracy |
|---|---:|
| Baseline CNN — MNIST test | 92.81% |
| Baseline CNN — webcam test (120 images) | 13.33% |
| Early fine-tuned CNN — webcam test | 20.83% |
| Feedback CNN — validation (31 images) | 77.42% |
| Feedback CNN — MNIST test | 91.22% |

The feedback CNN results above are from an earlier experiment using 146 labeled webcam examples, not the final 611-image training run.

The model was subsequently retrained with additional feedback data. Independent webcam accuracy for the final checkpoint has not yet been established.

**Key observation:** High benchmark accuracy does not necessarily translate to reliable webcam recognition. Lighting, background, hand orientation, and preprocessing differences can significantly affect performance.

## Project Files

- `src/` — recognition, model, hand tracking, inference and UI code
- `docs/` — project specification, attribution and release history
- `scripts/download_models.py` — downloads the required assets
- `models/feedback_candidate.pth` — trained CNN checkpoint, downloaded during setup
- `assets/hand_landmarker.task` — MediaPipe hand landmarker, downloaded during setup
- `requirements.txt` — Python dependencies
- `results/` — historical evaluation CSV files

**Note:** The model binary and confusion-matrix image are available in the
distributed project ZIP, but not in this GitHub subfolder. The downloader
recovers the runtime model from the original project source.

## Run locally (Windows 11)

Install **Python 3.12** and Git. Clone the project collection and use PowerShell:

```powershell
git clone https://github.com/Aime2k2/Projects.git
cd Projects\signframe-ai
py -3.12 -m venv .venv
.\.venv\Scripts\Activate.ps1
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
python scripts/download_models.py
python -m src.webcam_app
```

In Command Prompt you can activate the environment with
` .venv\Scripts\activate.bat ` instead. Allow Windows camera access.

For macOS/Linux, create a Python 3.12 environment with
`python3.12 -m venv .venv`, activate with `source .venv/bin/activate`,
and run the same last four `python` commands.

The application is a **desktop webcam app**, not a web page. Run commands
from inside the `signframe-ai` folder. If assets are missing, rerun
`python scripts/download_models.py`. If the camera cannot be opened,
check that another app is not holding the device.

The original code was tested on macOS. This packaged release passed
Python syntax checks and its CNN checkpoint loaded for a 24-class smoke
test, but the full webcam UI has not been tested on Windows here.

## Keyboard Controls

| Key | Action |
|---|---|
| Enter | Confirm predicted letter |
| Space | Insert space |
| Backspace | Delete character |
| E | Edit message |
| V | Speak message |
| F | Open communication display |
| C | Clear message |
| Q / Esc | Quit |

In editing mode, Enter saves changes and Esc cancels. In communication display mode, F or Esc returns to the dashboard.

## Limitations

- Supports static fingerspelling only; J and Z are not recognized.
- Does not recognize continuous ASL signing or grammar.
- Performance varies with lighting, background, and hand orientation.
- Feedback training data came from a single signer.
- Final-model accuracy on independent signers remains untested.
- Users must manually confirm predicted letters.

SignFrame AI is an experimental prototype, not a dependable sign language translation or accessibility-critical communication system.

## Future Work

- Improve recognition across different users and environments.
- Explore landmark-based and hybrid CNN architectures.
- Support dynamic letters J and Z.
- Investigate continuous fingerspelling recognition.
- Evaluate real-time latency and cross-platform compatibility.

## Credits and project provenance

- **Portfolio maintenance and release preparation:** Jean Aimé Iraguha ([Aime2k2](https://github.com/Aime2k2)); finalized **15 November 2026**.
- **Original project author and CNN implementation:** [Tayyaba Ilyas](https://github.com/tayyabastack), [upstream SignFrame AI](https://github.com/tayyabastack/signframe-ai).

The supplied project originally identified Tayyaba Ilyas as its author.
This portfolio package retains that credit; publishing it does not
establish independent authorship of the underlying implementation.
The upstream repository had no LICENSE file when reviewed; check with
the original author about redistribution permissions. See
`docs/ATTRIBUTION.md`.
