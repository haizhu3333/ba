from typing import Optional
import torch

class SimpleModel(torch.nn.Module):

    def __init__(self, size: int):
        super().__init__()
        self.net = torch.nn.Sequential(
            torch.nn.Linear(52, size),
            torch.nn.ReLU(),
            torch.nn.Linear(size, 1),
        )
        self.loss = torch.nn.MSELoss(reduction='sum')

    def forward(self, inputs: torch.Tensor, targets: Optional[torch.Tensor]) -> torch.Tensor:
        outputs = self.net(inputs).squeeze(dim=1)
        if targets is None:
            return outputs
        return self.loss(outputs, targets)
