
from pathlib import Path

import numpy as np
import torch
import matplotlib.pyplot as plt

from torch.utils.data import DataLoader
from sklearn.metrics import (
    accuracy_score,
    classification_report,
    confusion_matrix,
    ConfusionMatrixDisplay
)

from src.dataset import SignLanguageDataset, VALID_LABELS
from src.model import SignLanguageCNN


def main():
    device = torch.device(
        "mps" if torch.backends.mps.is_available() else "cpu"
    )

    print("Evaluation device:", device)

    # Load the held-out test dataset
    test_dataset = SignLanguageDataset(
        "data/raw/sign_mnist_test.csv"
    )

    test_loader = DataLoader(
        test_dataset,
        batch_size=64,
        shuffle=False
    )

    # Recreate the CNN architecture
    model = SignLanguageCNN().to(device)

    # Load learned weights from training
    model.load_state_dict(
        torch.load(
            "models/best_cnn.pth",
            map_location=device,
            weights_only=True
        )
    )

    # Switch to evaluation mode
    model.eval()

    all_predictions = []
    all_labels = []

    # Evaluate without updating model weights
    with torch.no_grad():
        for images, labels in test_loader:
            images = images.to(device)

            outputs = model(images)
            predictions = outputs.argmax(dim=1)

            all_predictions.extend(
                predictions.cpu().numpy()
            )
            all_labels.extend(
                labels.numpy()
            )

    all_predictions = np.array(all_predictions)
    all_labels = np.array(all_labels)

    accuracy = accuracy_score(
        all_labels, all_predictions
    )

    letters = [
        chr(ord("A") + label)
        for label in VALID_LABELS
    ]

    print(f"\nTest Accuracy: {accuracy * 100:.2f}%")

    print("\nClassification Report:")
    print(
        classification_report(
            all_labels,
            all_predictions,
            labels=list(range(24)),
            target_names=letters,
            zero_division=0
        )
    )

    # Generate confusion matrix
    cm = confusion_matrix(
        all_labels,
        all_predictions,
        labels=list(range(24))
    )

    fig, ax = plt.subplots(figsize=(13, 11))

    display = ConfusionMatrixDisplay(
        confusion_matrix=cm,
        display_labels=letters
    )

    display.plot(
        ax=ax,
        cmap="Blues",
        xticks_rotation=45,
        values_format="d",
        colorbar=False
    )

    ax.set_title("SignFrame AI — Test Confusion Matrix")
    plt.tight_layout()

    Path("results").mkdir(exist_ok=True)

    plt.savefig(
        "results/confusion_matrix.png",
        dpi=200,
        bbox_inches="tight"
    )

    print("\nConfusion matrix saved to results/confusion_matrix.png")

    plt.show()


if __name__ == "__main__":
    main()
