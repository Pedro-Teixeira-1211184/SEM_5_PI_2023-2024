import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import IRobotDTO from "../../dto/IRobotDTO";

@Injectable({
    providedIn: 'root'
})
export class RobotService {

    constructor() {
    }

    public async createRobot(robotType: string, code: string, serialNumber: string, nickname: string, brand: string, isActive: boolean): Promise<void> {
        try {
            const response = await fetch(Constants.API_ROBOT_CREATE_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    robotType: robotType,
                    code: code,
                    serialNumber: serialNumber,
                    nickname: nickname,
                    brand: brand,
                    isActive: isActive
                })
            });

            const json = await response.json();

            if (response.status === 201) {
                alert('Created robot successfully');
                window.location.href = '/home';
            } else {
                alert('Was not possible to create this robot');
            }
        } catch (e) {
            console.log(e);
        }
    }


    public async getAllRobots(): Promise<any> {
        try {
            const response = await fetch(Constants.API_ROBOT_GET_ALL_URL, {
                method: 'GET',
                headers: {
                    'Content-Type': 'application/json'
                }
            });

            const json = await response.json();

            if (response.status === 200) {
                return json;
            } else {
                alert(json);
            }
        } catch (e) {
            console.log(e);
        }
    }

    public async updateRobotStatus(robotCode: string): Promise<any> {
        try {
            const response = await fetch(Constants.API_ROBOT_UPDATE_STATUS_URL + robotCode, {
                method: 'PATCH',
                headers: {
                    'Content-Type': 'application/json'
                }
            });

            const json = await response.json();

            if (response.status === 200) {
                alert('Robot ' + robotCode + ' status updated!');
                return json;
            } else {
                alert(json);
            }
        } catch (e) {
            console.log(e);
        }
    }
}
