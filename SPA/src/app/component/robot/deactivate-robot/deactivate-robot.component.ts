import {Component, inject, OnInit} from '@angular/core';
import {RobotService} from "../../../services/robot/robot.service";
import IRobotDTO from "../dto/IRobotDTO";


@Component({
    selector: 'app-deactivate-robot',
    templateUrl: './deactivate-robot.component.html',
    styleUrls: ['./deactivate-robot.component.css']
})
export class DeactivateRobotComponent implements OnInit {

    service = inject(RobotService);
    robots: IRobotDTO[] = [];

    ngOnInit(): void {
    }

    constructor() {
        this.getAllRobots();
    }

    public async getAllRobots() {
        this.robots = await this.service.getAllRobots();
    }

    public async updateStatus(robot: IRobotDTO) {
        try {
            await this.service.updateRobotStatus(robot.code);
            await this.getAllRobots();
        } catch (e) {
            console.log(e);
        }
    }
}
