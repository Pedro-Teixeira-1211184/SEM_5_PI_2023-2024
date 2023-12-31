import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";

@Injectable({
  providedIn: 'root'
})
export class TaskService {

  constructor() {
  }

  public async getAllTaskTypes(): Promise<any> {
    try {
      let types: { designation: string, robotType: string }[] = [];
      const response = await fetch(Constants.API_TASK_TYPE_GET_ALL_URL, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json'
        }
      });
      const x = await response.json();
      for (let i = 0; i < x.length; i++) {
        types.push({designation: x[i].designation, robotType: x[i].robotType});
      }
      return types;
    } catch (e) {
      console.log(e);
    }
  }

  public async createTaskRequest(startFloorCode: string, startX: number, startY: number, endFloorCode: string, endX: number, endY: number, taskType: string, robotCode: string, description: string): Promise<void> {
    try {
      const response = await fetch(Constants.API_TASK_REQUEST_CREATE, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          UserEmail: localStorage.getItem('email'),
          StartX: startX,
          StartY: startY,
          StartFloorCode: startFloorCode,
          EndX: endX,
          EndY: endY,
          EndFloorCode: endFloorCode,
          Description: description,
          TaskType: taskType,
          RobotCode: robotCode,
          TaskState: "PENDING"
        })
      });

      if (response.status === 200) {
        alert('Created task request successfully');
        window.location.href = '/home';
      } else {
        alert('Was not possible to create this task request');
      }
    } catch (e) {
      console.log(e);
    }
  }
}
