
import random
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn

from torch.utils.data import DataLoader, random_split

from src.dataset import SignLanguageDataset
from src.model import SignLanguageCNN


# Reproducibility
SEED = 42
random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)

# Training configuration
BATCH_SIZE = 64
LEARNING_RATE = 0.001
EPOCHS = 15

# Select the best available device
device = torch.device(
    "mps" if torch.backends.mps.is_available() else "cpu"
)

print("Training device:", device)

# Load the training dataset
dataset = SignLanguageDataset(
    "data/raw/sign_mnist_train.csv"
)

# Split into 85% training and 15% validation
val_size = int(0.15 * len(dataset))
train_size = len(dataset) - val_size

train_dataset, val_dataset = random_split(
    dataset,
    [train_size, val_size],
    generator=torch.Generator().manual_seed(SEED)
)

train_loader = DataLoader(
    train_dataset,
    batch_size=BATCH_SIZE,
    shuffle=True
)

val_loader = DataLoader(
    val_dataset,
    batch_size=BATCH_SIZE,
    shuffle=False
)

print("Training samples:", train_size)
print("Validation samples:", val_size)

# Initialize CNN
model = SignLanguageCNN().to(device)

# Loss function and optimizer
criterion = nn.CrossEntropyLoss()
optimizer = torch.optim.Adam(
    model.parameters(),
    lr=LEARNING_RATE
)

# Save model checkpoints
Path("models").mkdir(exist_ok=True)
best_val_loss = float("inf")

# Training loop
for epoch in range(EPOCHS):

    model.train()
    train_loss = 0.0
    train_correct = 0

    for images, labels in train_loader:
        images = images.to(device)
        labels = labels.to(device)

        optimizer.zero_grad()

        outputs = model(images)
        loss = criterion(outputs, labels)

        loss.backward()
        optimizer.step()

        train_loss += loss.item() * images.size(0)

        predictions = outputs.argmax(dim=1)
        train_correct += (
            predictions == labels
        ).sum().item()

    train_loss /= train_size
    train_accuracy = train_correct / train_size

    # Validation
    model.eval()
    val_loss = 0.0
    val_correct = 0

    with torch.no_grad():
        for images, labels in val_loader:
            images = images.to(device)
            labels = labels.to(device)

            outputs = model(images)
            loss = criterion(outputs, labels)

            val_loss += loss.item() * images.size(0)

            predictions = outputs.argmax(dim=1)
            val_correct += (
                predictions == labels
            ).sum().item()

    val_loss /= val_size
    val_accuracy = val_correct / val_size

    print(
        f"Epoch {epoch + 1}/{EPOCHS} | "
        f"Train Loss: {train_loss:.4f} | "
        f"Train Acc: {train_accuracy:.4f} | "
        f"Val Loss: {val_loss:.4f} | "
        f"Val Acc: {val_accuracy:.4f}"
    )

    # Save the model with the lowest validation loss
    if val_loss < best_val_loss:
        best_val_loss = val_loss

        torch.save(
            model.state_dict(),
            "models/best_cnn.pth"
        )

        print("  Best model saved!")

print("\nTraining completed.")
print("Best validation loss:", round(best_val_loss, 4))
