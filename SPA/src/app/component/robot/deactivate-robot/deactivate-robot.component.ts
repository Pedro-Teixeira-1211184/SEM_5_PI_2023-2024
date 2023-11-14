import {Component, inject, OnInit} from '@angular/core';
import {RobotService} from "../../../services/robot/robot.service";


@Component({
    selector: 'app-deactivate-robot',
    templateUrl: './deactivate-robot.component.html',
    styleUrls: ['./deactivate-robot.component.css']
})
export class DeactivateRobotComponent implements OnInit {

    service = inject(RobotService);
    ngOnInit(): void {
    }

    constructor() {
    }

}
