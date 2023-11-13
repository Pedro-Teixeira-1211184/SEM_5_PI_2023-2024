import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import { Observable, from } from 'rxjs';

@Injectable({
    providedIn: 'root'
})

export class FloorService {

    constructor() {
    }

    public async createFloor(buildingCode: string, number: number, description: string): Promise<void> {
        try{
            let hasDescription = true;

            //check if description is empty
            if (description === '' || description === null || description === undefined) {
                hasDescription = false;
            }

            const response = await this.getResponse(hasDescription, buildingCode, number, description);

            const json = await response.json();

            if (response.status === 201) {
                alert('Created floor successfully');
                window.location.href = '/home';
            } else {
                alert(json);
            }
        }catch (e) {
            console.log(e);
        }

    }

    /*public getFloorsByBuildingCode(buildingCode: string): Observable<any[]> {
        const response = fetch(Constants.API_FLOOR_GET_BY_BUILDING_CODE_URL + buildingCode, {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json'
            }
        });

        return from(response.then(res => res.json()));
    }*/ 


    private async getResponse(hasDescription: boolean, buildingCode: string, number: number, description: string) {

        if (hasDescription) {
            return await fetch(Constants.API_FLOOR_CREATE_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    buildingCode: buildingCode,
                    number: number,
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
                    number: number
                })
            });
        }
    }
}