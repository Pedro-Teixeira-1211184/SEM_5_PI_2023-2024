import {Component, inject, OnInit} from '@angular/core';
import {TaskService} from "../../../services/task/task.service";
import {ITaskRequestDTO} from "../../../dto/ITaskRequestDTO";

@Component({
  selector: 'app-task-request-update',
  templateUrl: './task-request-update.component.html',
  styleUrls: ['./task-request-update.component.css']
})
export class TaskRequestUpdateComponent implements OnInit {
  service: TaskService = inject(TaskService);

  requests: ITaskRequestDTO [] = [];

  constructor() {
  }

  ngOnInit(): void {
    this.getAllRequests();
  }

  private async getAllRequests(): Promise<void> {
    this.requests = await this.service.getPendingTaskRequests();
  }

  public async accept(request: ITaskRequestDTO): Promise<void> {
    await this.service.updateTaskRequest(request.id, "ACCEPTED");
    this.requests = [];
    await this.getAllRequests();
  }

  public async deny(request: ITaskRequestDTO): Promise<void> {
    await this.service.updateTaskRequest(request.id, "DENIED");
    this.requests = [];
    await this.getAllRequests();
  }
}
