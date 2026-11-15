
import torch
import torch.nn as nn


class SignLanguageCNN(nn.Module):

    def __init__(self):
        super().__init__()

        self.features = nn.Sequential(
            # First convolutional block
            nn.Conv2d(1, 32, kernel_size=3, padding=1),
            nn.ReLU(),
            nn.MaxPool2d(kernel_size=2),

            # Second convolutional block
            nn.Conv2d(32, 64, kernel_size=3, padding=1),
            nn.ReLU(),
            nn.MaxPool2d(kernel_size=2)
        )

        self.classifier = nn.Sequential(
            nn.Flatten(),
            nn.Linear(64 * 7 * 7, 128),
            nn.ReLU(),
            nn.Dropout(0.3),
            nn.Linear(128, 24)
        )

    def forward(self, x):
        x = self.features(x)
        x = self.classifier(x)
        return x


if __name__ == "__main__":
    model = SignLanguageCNN()

    # Create a fake batch of 4 grayscale images
    sample_input = torch.randn(4, 1, 28, 28)

    # Pass images through the CNN
    output = model(sample_input)

    print("Input shape:", sample_input.shape)
    print("Output shape:", output.shape)
    print("Trainable parameters:", sum(
        p.numel() for p in model.parameters()
        if p.requires_grad
    ))
