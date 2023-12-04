import {Component, inject, OnInit} from '@angular/core';
import {TaskService} from "../../../services/task/task.service";

@Component({
  selector: 'app-task-request',
  templateUrl: './task-request.component.html',
  styleUrls: ['./task-request.component.css']
})
export class TaskRequestComponent implements OnInit {

  service: TaskService = inject(TaskService);

  tasks = [
    {id: 1, name: 'clean hall'},
    {id: 2, name: 'surveillance'},
    {id: 3, name: 'pickup'},
    {id: 4, name: 'image acquisition'},
    {id: 5, name: 'clean windows'}
  ];

  constructor() {
  }

  ngOnInit(): void {
  }

}
