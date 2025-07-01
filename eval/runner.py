from dataclasses import dataclass
from pathlib import Path
from typing import Optional
import time
import torch
from torch.utils.data import DataLoader, TensorDataset

import models
import utils

def load_dataset(device: str, csv_path: Path) -> TensorDataset:
    cards, tricks = utils.load_data(csv_path)
    return TensorDataset(
        torch.tensor(cards.values, dtype=torch.float, device=device),
        torch.tensor(tricks.values, dtype=torch.float, device=device))

@dataclass
class Trainer:
    train_data: DataLoader
    test_data: DataLoader
    model: torch.nn.Module
    optimizer: torch.optim.Optimizer
    lr_scheduler: torch.optim.lr_scheduler.LRScheduler

    def train_epoch(self) -> float:
        total_loss = 0
        total_count = 0
        self.model.train()
        for inputs, targets in self.train_data:
            loss = self.model(inputs, targets)
            self.optimizer.zero_grad()
            loss.backward()
            self.optimizer.step()
            total_loss += loss.item()
            total_count += len(inputs)
        return total_loss / total_count

    @torch.inference_mode()
    def test_epoch(self) -> float:
        total_loss = 0
        total_count = 0
        self.model.eval()
        for inputs, targets in self.test_data:
            total_loss += self.model(inputs, targets)
            total_count += len(inputs)
        return total_loss / total_count

    def training_loop(self, epochs: int):
        start_time = time.time()
        for epoch in range(epochs):
            train_loss = self.train_epoch()
            test_loss = self.test_epoch()
            lrs = ', '.join(f'{lr:.3e}' for lr in self.lr_scheduler.get_last_lr())
            self.lr_scheduler.step()
            end_time = time.time()
            duration = end_time - start_time
            print(f'Epoch {epoch:3} | {duration:6.3f}s | lr {lrs} | '
                  f'loss {train_loss:9.4f} {test_loss:9.4f}')
            start_time = end_time


@dataclass
class Config:
    model_size: int = 100
    initial_lr: float = 0.001
    batch_size: int = 128
    test_batch_size: int = 400
    epochs: int = 50


def main(config: Config):
    device = 'cuda:0' if torch.cuda.is_available() else 'cpu'
    print(f'Using device: {device}')
    train_loader = DataLoader(
        load_dataset(device, utils.DATA_PATH), config.batch_size, shuffle=True)
    test_loader = DataLoader(
        load_dataset(device, utils.TEST_PATH), config.test_batch_size, shuffle=False)
    print(f'Data loaded')
    model = models.SimpleModel(config.model_size).to(device)
    optimizer = torch.optim.AdamW(model.parameters(), config.initial_lr)
    lr_scheduler = torch.optim.lr_scheduler.MultiStepLR(optimizer, [])
    trainer = Trainer(train_loader, test_loader, model, optimizer, lr_scheduler)
    trainer.training_loop(config.epochs)

if __name__ == '__main__':
    main(Config())
