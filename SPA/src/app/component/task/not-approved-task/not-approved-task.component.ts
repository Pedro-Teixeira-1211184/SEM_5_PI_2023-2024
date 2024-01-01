import {Component, inject} from '@angular/core';
import {ITaskRequestDTO} from "../../../dto/ITaskRequestDTO";
import {TaskService} from "../../../services/task/task.service";

@Component({
  selector: 'app-not-approved-task',
  templateUrl: './not-approved-task.component.html',
  styleUrls: ['./not-approved-task.component.css']
})
export class NotApprovedTaskComponent {
  service: TaskService = inject(TaskService);
  tasks: ITaskRequestDTO[] = [];

  constructor() {
    this.getNotApprovedTasks();
  }

  ngOnInit(): void {
  }

  public async getNotApprovedTasks() {
    this.tasks = await this.service.getNotApprovedTaskRequests();
  }
}
