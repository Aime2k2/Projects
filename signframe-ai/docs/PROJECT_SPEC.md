
# SignFrame AI — Project Specification

**Version:** 1.0  
**Status:** Historical design specification; portfolio package finalized 15 November 2026  
**Project Type:** Personal Deep Learning and Computer Vision Portfolio Project

---

## 1. Project Overview

SignFrame AI is a real-time American Sign Language (ASL) fingerspelling recognition application.

The system uses a webcam to detect a user's hand, visualize its landmarks, classify static ASL handshapes using a custom-trained Convolutional Neural Network (CNN), and compose text from confirmed predictions.

The entire application runs locally.

## 2. Project Objectives

1. Train a custom CNN using PyTorch.
2. Recognize 24 static ASL alphabet handshapes.
3. Integrate CNN inference with a live webcam.
4. Detect and track one hand using MediaPipe.
5. Display a green bounding box around the hand.
6. Display a green hand skeleton with 21 red landmark points.
7. Show predicted letters and confidence scores.
8. Stabilize predictions across consecutive frames.
9. Allow users to compose text from confirmed letters.
10. Evaluate performance on both the original dataset and webcam images.

## 3. Technology Stack

| Technology | Responsibility |
|------------|----------------|
| Python 3.12 | Programming language |
| PyTorch | CNN training and inference |
| OpenCV | Webcam capture and interface |
| MediaPipe | Hand detection and landmarks |
| NumPy | Image processing |
| pandas | Dataset loading |
| scikit-learn | Model evaluation |
| Matplotlib | Evaluation visualizations |
| Git | Version control |
| GitHub | Project hosting |
| VS Code | Development environment |

No paid APIs, cloud services, databases, or external inference services are required.

## 4. Dataset

**Dataset:** Sign Language MNIST

Source: https://www.kaggle.com/datasets/datamunge/sign-language-mnist

### Dataset Properties

- Training images: 27,455
- Test images: 7,172
- Resolution: 28 × 28
- Channels: 1 (grayscale)
- Classes: 24
- Unsupported letters: J and Z

### Data Preparation

1. Load the training and testing CSV files.
2. Extract image pixels and labels.
3. Reshape images to 28 × 28.
4. Normalize pixel values.
5. Create a stratified training/validation split.
6. Convert images to PyTorch tensors.
7. Create DataLoaders for training, validation, and testing.

The provided test set must remain separate from training and validation.

## 5. CNN Architecture

### Input

Grayscale image with shape:

1 × 28 × 28

### Layers

1. Conv2D: 32 filters, kernel 3 × 3, padding 1
2. ReLU
3. MaxPool2D: 2 × 2
4. Conv2D: 64 filters, kernel 3 × 3, padding 1
5. ReLU
6. MaxPool2D: 2 × 2
7. Flatten
8. Linear: 3136 → 128
9. ReLU
10. Dropout: 0.3
11. Linear: 128 → 24

### Training Configuration

| Parameter | Value |
|-----------|-------|
| Framework | PyTorch |
| Loss | CrossEntropyLoss |
| Optimizer | Adam |
| Learning rate | 0.001 |
| Batch size | 64 |
| Maximum epochs | 15 |
| Validation split | 15% |
| Random seed | 42 |
| Best checkpoint | Lowest validation loss |
| Training device | Apple MPS if available; otherwise CPU |

The model will be trained from scratch rather than using a pretrained ASL classifier.

## 6. Hand Tracking

MediaPipe will detect and track one hand.

The system will estimate 21 hand landmarks, including the wrist, finger joints, and fingertips.

### Visualization

- Green bounding box around the hand.
- Green connections between landmarks.
- Red circles at landmark positions.
- Live tracking updates as the hand moves.

MediaPipe is used for hand localization and visualization.

The custom CNN performs ASL letter classification.

## 7. Real-Time Recognition Pipeline

1. Capture a frame from the webcam.
2. Detect hand landmarks using MediaPipe.
3. Calculate the hand bounding box.
4. Crop the original, unannotated hand image.
5. Convert the crop to grayscale.
6. Resize it to 28 × 28.
7. Normalize the image.
8. Convert it to a PyTorch tensor.
9. Run CNN inference.
10. Calculate class probabilities using softmax.
11. Identify the predicted letter.
12. Update prediction stability.
13. Draw the hand skeleton and interface.
14. Display the updated webcam frame.

The CNN must not receive images containing the drawn red dots or green skeleton.

## 8. Prediction Stabilization

A rolling window of recent predictions will be maintained.

A letter becomes stable only when enough recent predictions agree and satisfy the configured confidence threshold.

### Rules

- Low-confidence predictions are considered uncertain.
- Missing-hand detection clears the current stable prediction.
- Only stable letters can be appended to the message.
- Predictions are not automatically appended every frame.
- Thresholds are configurable and tested using webcam development examples.

## 9. Text Composition

The application maintains a text buffer.

### Keyboard Controls

| Key | Action |
|-----|--------|
| A | Append the current stable letter |
| Space | Insert a word space |
| Backspace | Delete the previous character |
| C | Clear the complete message |
| Q | Quit the application |

Repeated letters require separate confirmation actions.

## 10. Application Interface

The OpenCV application window will contain:

- Live webcam feed.
- Green hand bounding box.
- Green hand skeleton.
- Red landmark points.
- Predicted letter.
- Model confidence.
- Prediction stability status.
- Composed text.
- Keyboard instructions.

The interface will use a dark visual theme.

No web frontend or mobile application is included.

## 11. Evaluation

### Dataset Evaluation

Metrics:

- Accuracy
- Macro precision
- Macro recall
- Macro F1-score
- Confusion matrix
- Training loss
- Validation loss

### Webcam Evaluation

A separate labeled webcam test set will be collected.

The evaluation will investigate:

- Recognition accuracy on real webcam images.
- Confusion between similar handshapes.
- Effects of lighting and hand positioning.
- Missing-hand handling.
- False confident predictions.
- Approximate inference speed.

Dataset accuracy and webcam accuracy must be reported separately.

## 12. Source Files

| File | Responsibility |
|------|----------------|
| dataset.py | Dataset loading and DataLoaders |
| model.py | CNN architecture |
| train.py | Model training |
| evaluate.py | Model evaluation |
| hand_tracker.py | MediaPipe integration |
| preprocessing.py | Webcam image preprocessing |
| inference.py | CNN inference |
| stabilizer.py | Prediction stabilization |
| text_composer.py | Text buffer and controls |
| visualizer.py | Webcam overlays and interface |
| webcam_app.py | Main application |

## 13. Testing

Automated tests will cover:

- Image preprocessing.
- CNN input/output shapes.
- Prediction stabilization.
- Text composition controls.

Manual tests will cover:

- Webcam access.
- Hand detection.
- Landmark visualization.
- Live recognition.
- Keyboard interaction.
- Application shutdown.

## 14. Project Limitations

The system will not support:

- Dynamic ASL letters J and Z.
- Full ASL sentence translation.
- Sign language grammar understanding.
- Multiple-hand recognition.
- Speech synthesis.
- Cloud-based inference.
- Mobile or browser deployment.
- Automatic hands-free letter confirmation.

This is an educational prototype, not a validated assistive communication system.

## 15. Development Schedule

| Day | Task |
|-----|------|
| 1 | Setup, documentation, dataset preparation |
| 2 | CNN implementation and training |
| 3 | Model evaluation and preprocessing |
| 4 | Webcam and MediaPipe tracking |
| 5 | CNN integration and text composition |
| 6 | Testing, documentation, GitHub, demo |

## 16. Completion Criteria

The project is complete when:

1. The CNN is trained and saved.
2. Dataset evaluation metrics are generated.
3. The webcam application runs.
4. Hand landmarks and bounding box are visualized.
5. The trained CNN generates live predictions.
6. Stable letters can be confirmed and added to text.
7. Webcam evaluation results are documented.
8. The GitHub repository contains reproducible instructions.
9. A real demonstration video is recorded.

## 17. Scope Control

The architecture, project objectives, and supported features defined in this document form the agreed version 1 scope.

Necessary bug fixes and compatibility adjustments are permitted.

Additional features or major architectural changes require explicit approval.

---

**Project Name:** SignFrame AI  
**Version:** 1.0  
**Status:** In Development
