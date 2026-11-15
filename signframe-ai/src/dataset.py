
import pandas as pd
import numpy as np
import torch
from torch.utils.data import Dataset


# Convert original ASL labels into 24 consecutive classes
VALID_LABELS = [i for i in range(26) if i not in (9, 25)]
LABEL_TO_INDEX = {
    label: index for index, label in enumerate(VALID_LABELS)
}


class SignLanguageDataset(Dataset):

    def __init__(self, csv_path):
        # Read the dataset
        df = pd.read_csv(csv_path)

        # Separate labels from pixel values
        original_labels = df["label"].to_numpy()
        pixels = df.drop(columns=["label"]).to_numpy(
            dtype=np.float32
        )

        # Normalize pixel values from 0-255 to 0-1
        pixels = pixels / 255.0

        # Reshape into (samples, channels, height, width)
        pixels = pixels.reshape(-1, 1, 28, 28)

        # Convert images into PyTorch tensors
        self.images = torch.from_numpy(pixels)

        # Map original labels into indices 0-23
        mapped_labels = [
            LABEL_TO_INDEX[int(label)]
            for label in original_labels
        ]

        self.labels = torch.tensor(
            mapped_labels, dtype=torch.long
        )

    def __len__(self):
        return len(self.labels)

    def __getitem__(self, index):
        return self.images[index], self.labels[index]


if __name__ == "__main__":
    dataset = SignLanguageDataset(
        "data/raw/sign_mnist_train.csv"
    )

    image, label = dataset[0]

    print("Total samples:", len(dataset))
    print("Image shape:", image.shape)
    print("Image dtype:", image.dtype)
    print("Label:", label.item())
    print("Pixel range:", image.min().item(), image.max().item())
    print("Number of classes:", len(VALID_LABELS))
