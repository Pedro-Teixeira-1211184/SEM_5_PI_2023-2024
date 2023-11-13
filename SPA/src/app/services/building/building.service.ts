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

    public async editBuilding(code: string, length: number, width: number, name: string, description: string, maxFloors: number, minFloors: number): Promise<void> {
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

            const response = await this.getResponseEdit(hasName, hasDescription, code, length, width, name, description, maxFloors, minFloors);

            const json = await response.json();

            if (response.status === 200) {
                alert('Edited building successfully');
                window.location.href = '/home';
            } else {
                alert(json);
            }
        } catch (e) {
            console.log(e);
        }
    }


    private async getResponseEdit(hasName: boolean, hasDescription: boolean, code: string, length: number, width: number, name: string, description: string, maxFloors: number, minFloors: number) {

        if (hasName && hasDescription) {
            return await fetch(Constants.API_BUILDING_EDIT_URL, {
                method: 'PUT',
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
            return await fetch(Constants.API_BUILDING_EDIT_URL, {
                method: 'PUT',
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
            return await fetch(Constants.API_BUILDING_EDIT_URL, {
                method: 'PUT',
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

        return await fetch(Constants.API_BUILDING_EDIT_URL, {
            method: 'PUT',
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

    public async getAllBuildingsCode(): Promise<any> {
        try {
            let codes: string[] = [];
            const response = await fetch(Constants.API_BUILDING_GET_ALL_URL, {
                method: 'GET',
                headers: {
                    'Content-Type': 'application/json'
                }
            });
            const x = await response.json();
            for (let i = 0; i < x.length; i++) {
                codes.push(x[i].code);
            }
            return codes;
        } catch (e) {
            console.log(e);
        }
    }

}

