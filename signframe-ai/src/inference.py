
from pathlib import Path

import torch

from src.model import SignLanguageCNN
from src.dataset import VALID_LABELS


MODEL_PATH = Path("models/feedback_candidate.pth")


class SignPredictor:
    def __init__(self):
        if not MODEL_PATH.exists():
            raise FileNotFoundError(
                f"Trained CNN not found: {MODEL_PATH}"
            )

        self.device = torch.device(
            "mps"
            if torch.backends.mps.is_available()
            else "cuda"
            if torch.cuda.is_available()
            else "cpu"
        )

        self.model = SignLanguageCNN().to(self.device)

        checkpoint = torch.load(
            MODEL_PATH,
            map_location=self.device,
            weights_only=True
        )

        if (
            isinstance(checkpoint, dict)
            and "model_state_dict" in checkpoint
        ):
            checkpoint = checkpoint["model_state_dict"]

        self.model.load_state_dict(checkpoint)
        self.model.eval()

        print(
            f"SignFrame AI model loaded: {MODEL_PATH}"
        )
        print(f"Inference device: {self.device}")

    def predict(self, image_tensor):
        if image_tensor is None:
            return None, 0.0

        image_tensor = image_tensor.to(self.device)

        if image_tensor.ndim == 3:
            image_tensor = image_tensor.unsqueeze(0)

        with torch.inference_mode():
            outputs = self.model(image_tensor)

            probabilities = torch.softmax(
                outputs,
                dim=1
            )

            confidence, predicted_index = (
                probabilities.max(dim=1)
            )

        index = predicted_index.item()
        original_label = VALID_LABELS[index]

        letter = chr(
            ord("A") + original_label
        )

        return letter, confidence.item()
