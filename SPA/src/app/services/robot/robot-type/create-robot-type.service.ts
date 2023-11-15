import {Injectable} from '@angular/core';
import Constants from "../../../../utils/Constants";

@Injectable({
    providedIn: 'root'
})
export class CreateRobotTypeService {

    constructor() {
    }

    public async createRobotType(value: string, selectedTasks: string[]) {
        try {
            let response = await fetch(Constants.API_ROBOT_TYPE_CREATE_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    designation: value
                })
            });

            let json = await response.json();

            if (response.status === 201) {
                alert('Robot type ' + value + ' created!');
                for (const task of selectedTasks) {
                    response = await fetch(Constants.API_TASK_TYPE_CREATE_URL, {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json'
                        },
                        body: JSON.stringify({
                            designation: task,
                            robotType: value
                        })
                    });
                    json = await response.json();
                    if (response.status === 201) {
                        alert(task + ' added to ' + value + '!');
                    } else {
                        alert(json + ' in ' + value + ' - ' + task);
                    }
                }
            } else {
                alert(json + ' - ' + value);
                for (const task of selectedTasks) {
                    response = await fetch(Constants.API_TASK_TYPE_CREATE_URL, {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json'
                        },
                        body: JSON.stringify({
                            designation: task,
                            robotType: value
                        })
                    });
                    json = await response.json();
                    if (response.status === 201) {
                        alert(task + ' added to ' + value + '!');
                    } else {
                        alert(json + ' in ' + value + ' - ' + task);
                    }
                }
            }
        } catch (e) {
            console.log(e);
        }
    }
}
