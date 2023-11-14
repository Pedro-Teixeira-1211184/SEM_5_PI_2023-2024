import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import IRobotDTO from "../../component/robot/dto/IRobotDTO";

@Injectable({
    providedIn: 'root'
})
export class RobotService {

    constructor() {
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
