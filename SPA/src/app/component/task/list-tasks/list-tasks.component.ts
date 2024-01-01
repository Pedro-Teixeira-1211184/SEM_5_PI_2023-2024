import {Component, inject} from '@angular/core';
import {TaskService} from "../../../services/task/task.service";
import {ITaskRequestDTO} from "../../../dto/ITaskRequestDTO";
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {RobotService} from "../../../services/robot/robot.service";

@Component({
  selector: 'app-list-tasks',
  templateUrl: './list-tasks.component.html',
  styleUrls: ['./list-tasks.component.css']
})
export class ListTasksComponent {
  service: TaskService = inject(TaskService);
  r_service: RobotService = inject(RobotService);

  // task requests DTO + robot type
  tasks: { taskRequest: ITaskRequestDTO, robotType: string }[] = [];
  tasksPresented: { taskRequest: ITaskRequestDTO, robotType: string }[] = [];
  emails: string[] = [];
  types: string[] = [];
  states: string[] = [];

  form!: FormGroup;

  constructor() {
    this.getTasks();
  }

  ngOnInit(): void {
    this.form = new FormGroup({
      user: new FormControl('', [Validators.required]),
      robotType: new FormControl('', [Validators.required]),
      state: new FormControl('', [Validators.required]),
    });
  }

  get user() {
    return this.form.get('user');
  }

  get robotType() {
    return this.form.get('robotType');
  }

  get state() {
    return this.form.get('state');
  }

  public async getTasks() {
    await this.updateTasks();
    for (let task of this.tasks) {
      if (!this.emails.includes(task.taskRequest.userEmail)) {
        this.emails.push(task.taskRequest.userEmail);
      }
      const robot = await this.r_service.getRobotByCode(task.taskRequest.robotCode);
      if (!this.types.includes(robot.robotType)) {
        this.types.push(robot.robotType);
      }
      if (!this.states.includes(task.taskRequest.taskState)) {
        this.states.push(task.taskRequest.taskState);
      }
    }
    this.tasksPresented = this.tasks;
  }

  async updateTasks() {
    const tasks = await this.service.getAllTaskRequests();
    this.tasks = [];
    for (let task of tasks) {
      const robot = await this.r_service.getRobotByCode(task.robotCode);
      this.tasks.push({taskRequest: task, robotType: robot.robotType});
    }
  }

  emailSearch() {
    this.tasksPresented = [];
    for (let task of this.tasks) {
      if (task.taskRequest.userEmail === this.user?.value) {
        this.tasksPresented.push(task);
      }
    }
  }

  robotTypeSearch() {
    this.tasksPresented = [];
    for (let task of this.tasks) {
      if (task.robotType === this.robotType?.value) {
        this.tasksPresented.push(task);
      }
    }
  }

  stateSearch() {
    this.tasksPresented = [];
    for (let task of this.tasks) {
      if (task.taskRequest.taskState === this.state?.value) {
        this.tasksPresented.push(task);
      }
    }
  }
}
