import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";

@Injectable({
    providedIn: 'root'
})
export class BuildingService {

    constructor() {
    }


    public async createBuilding(code: string, length: number, width: number, name: string, description: string, maxFloors: number, minFloors: number): Promise<void> {
        try {

            let hasName = true;
            let hasDescription = true;

            //check if name is empty
            if (name === '' || name === null || name === undefined) {
                hasName = false;
            }
            //check if description is empty
            if (description === '' || description === null || description === undefined) {
                hasDescription = false;
            }

            const response = await this.getResponse(hasName, hasDescription, code, length, width, name, description, maxFloors, minFloors);

            const json = await response.json();

            if (response.status === 200) {
                alert('Created building successfully');
                window.location.href = '/home';
            } else {
                alert(json);
            }
        } catch (e) {
            console.log(e);
        }
    }

    private async getResponse(hasName: boolean, hasDescription: boolean, code: string, length: number, width: number, name: string, description: string, maxFloors: number, minFloors: number) {

        if (hasName && hasDescription) {
            return await fetch(Constants.API_BUILDING_CREATE_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    code: code,
                    dimensions: {
                        length: length,
                        width: width
                    },
                    name: name,
                    description: description,
                    maxFloors: maxFloors,
                    minFloors: minFloors
                })
            });
        }

        if (hasName && !hasDescription) {
            return await fetch(Constants.API_BUILDING_CREATE_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    code: code,
                    dimensions: {
                        length: length,
                        width: width
                    },
                    name: name,
                    maxFloors: maxFloors,
                    minFloors: minFloors
                })
            });
        }
        if (!hasName && hasDescription) {
            return await fetch(Constants.API_BUILDING_CREATE_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    code: code,
                    dimensions: {
                        length: length,
                        width: width
                    },
                    description: description,
                    maxFloors: maxFloors,
                    minFloors: minFloors
                })
            });
        }

        return await fetch(Constants.API_BUILDING_CREATE_URL, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                code: code,
                dimensions: {
                    length: length,
                    width: width
                },
                maxFloors: maxFloors,
                minFloors: minFloors
            })
        });
    }

}
