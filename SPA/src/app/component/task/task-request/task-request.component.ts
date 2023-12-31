import {Component, inject, OnInit} from '@angular/core';
import {TaskService} from "../../../services/task/task.service";
import {FloorService} from "../../../services/floor/floor.service";
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {BuildingService} from "../../../services/building/building.service";
import {RobotService} from "../../../services/robot/robot.service";
import IRobotDTO from "../../../dto/IRobotDTO";

@Component({
  selector: 'app-task-request',
  templateUrl: './task-request.component.html',
  styleUrls: ['./task-request.component.css']
})
export class TaskRequestComponent implements OnInit {

  service: TaskService = inject(TaskService);
  b_service: BuildingService = inject(BuildingService);
  f_service: FloorService = inject(FloorService);
  r_service: RobotService = inject(RobotService);

  taskTypes: { designation: string, robotType: string }[] = [];
  taskTypesPresented: string[] = [];
  buildingsCodes: string[] = [];
  floorsCode: { code: string, description: string }[] = [];
  filteredRobots: IRobotDTO[] = [];

  taskForm!: FormGroup;

  constructor() {
    this.getBuildingsCodes();
    this.getTaskTypes();
  }

  ngOnInit(): void {
    this.taskForm = new FormGroup({
      startFloorCode: new FormControl('', [Validators.required]),
      startX: new FormControl('', [Validators.required]),
      startY: new FormControl('', [Validators.required]),
      endFloorCode: new FormControl('', [Validators.required]),
      endX: new FormControl('', [Validators.required]),
      endY: new FormControl('', [Validators.required]),
      taskType: new FormControl('', [Validators.required]),
      robotCode: new FormControl('', [Validators.required]),
      description: new FormControl('', [Validators.required])
    });
  }

  get startFloorCode() {
    return this.taskForm.get('startFloorCode');
  }

  get startX() {
    return this.taskForm.get('startX');
  }

  get startY() {
    return this.taskForm.get('startY');
  }

  get endFloorCode() {
    return this.taskForm.get('endFloorCode');
  }

  get endX() {
    return this.taskForm.get('endX');
  }

  get endY() {
    return this.taskForm.get('endY');
  }

  get taskType() {
    return this.taskForm.get('taskType');
  }

  get robotCode() {
    return this.taskForm.get('robotCode');
  }

  get description() {
    return this.taskForm.get('description');
  }

  private async getBuildingsCodes() {
    this.buildingsCodes = await this.b_service.getAllBuildingsCode();
    for (let buildingCode of this.buildingsCodes) {
      const x = await this.f_service.getFloorsByBuildingCode(buildingCode);
      for (let floor of x) {
        this.floorsCode.push({code: floor.code, description: floor.description});
      }
    }
  }

  private async getTaskTypes() {
    this.taskTypes = await this.service.getAllTaskTypes();
    for (let taskType of this.taskTypes) {
      if (!this.taskTypesPresented.includes(taskType.designation)) {
        this.taskTypesPresented.push(taskType.designation);
      }
    }
  }

  public async getAvailableRobots() {
    const robots = await this.r_service.getAllRobots();
    const taskType = this.taskForm.get('taskType')?.value;
    this.filteredRobots = [];
    const robotTypes = this.taskTypes.filter(task => task.designation === taskType);
    for (let robotType of robotTypes) {
      for (let robot of robots) {
        if (robot.robotType === robotType.robotType) {
          this.filteredRobots.push(robot);
        }
      }
    }
  }

  public async submit() {
    if (this.taskForm.get('startFloorCode')?.value !== null && this.taskForm.get('startFloorCode')?.value !== undefined && this.taskForm.get('startFloorCode')?.value !== ''
      && this.taskForm.get('startX')?.value !== null && this.taskForm.get('startX')?.value !== undefined && this.taskForm.get('startX')?.value !== ''
      && this.taskForm.get('startY')?.value !== null && this.taskForm.get('startY')?.value !== undefined && this.taskForm.get('startY')?.value !== ''
      && this.taskForm.get('endFloorCode')?.value !== null && this.taskForm.get('endFloorCode')?.value !== undefined && this.taskForm.get('endFloorCode')?.value !== ''
      && this.taskForm.get('endX')?.value !== null && this.taskForm.get('endX')?.value !== undefined && this.taskForm.get('endX')?.value !== ''
      && this.taskForm.get('endY')?.value !== null && this.taskForm.get('endY')?.value !== undefined && this.taskForm.get('endY')?.value !== ''
      && this.taskForm.get('taskType')?.value !== null && this.taskForm.get('taskType')?.value !== undefined && this.taskForm.get('taskType')?.value !== ''
      && this.taskForm.get('robotCode')?.value !== null && this.taskForm.get('robotCode')?.value !== undefined && this.taskForm.get('robotCode')?.value !== ''
      && this.taskForm.get('description')?.value !== null && this.taskForm.get('description')?.value !== undefined && this.taskForm.get('description')?.value !== '') {
      const startFloorCode = this.taskForm.get('startFloorCode')?.value;
      const startX = this.taskForm.get('startX')?.value;
      const startY = this.taskForm.get('startY')?.value;
      const endFloorCode = this.taskForm.get('endFloorCode')?.value;
      const endX = this.taskForm.get('endX')?.value;
      const endY = this.taskForm.get('endY')?.value;
      const taskType = this.taskForm.get('taskType')?.value;
      const robotCode = this.taskForm.get('robotCode')?.value;
      const description = this.taskForm.get('description')?.value;
      await this.service.createTaskRequest(startFloorCode, startX, startY, endFloorCode, endX, endY, taskType, robotCode, description);
    } else {
      alert('Invalid fields');
    }
  }
}
