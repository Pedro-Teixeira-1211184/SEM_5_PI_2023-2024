import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";

@Injectable({
    providedIn: 'root'
})

export class FloorService {

    constructor() {
    }

    public async createFloor(buildingCode: string, number: number, code: string, description: string): Promise<void> {
        try{
            let hasDescription = true;

            //check if description is empty
            if (description === '' || description === null || description === undefined) {
                hasDescription = false;
            }

            const response = await this.getResponse(hasDescription, buildingCode, number, code, description);

            const json = await response.json();

            if (response.status === 200) {
                alert('Created floor successfully');
                window.location.href = '/home';
            } else {
                alert(json);
            }
        }catch (e) {
            console.log(e);
        }

    }

    private async getResponse(hasDescription: boolean, buildingCode: string, number: number, code: string, description: string) {

        if (hasDescription) {
            return await fetch(Constants.API_FLOOR_CREATE_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    buildingCode: buildingCode,
                    number: number,
                    code: code,
                    description: description
                })
            });
        } else {
            return await fetch(Constants.API_FLOOR_CREATE_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    buildingCode: buildingCode,
                    number: number,
                    code: code
                })
            });
        }
    }
}