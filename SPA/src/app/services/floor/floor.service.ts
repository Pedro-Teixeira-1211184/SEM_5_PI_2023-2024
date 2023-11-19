import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import {from, Observable} from 'rxjs';
import IFloorDTO from "../../dto/IFloorDTO";

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

    public async getFloorsByBuildingCode(buildingCode: string): Promise<any> {
       try{
        let floors: IFloorDTO[] = [];
        const response = await fetch(Constants.API_FLOOR_GET_BY_BUILDING_CODE_URL + buildingCode, {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json'
            }
        });
        const x = await response.json();
        for (let i = 0; i < x.length; i++) {
            floors.push(x[i]);
        }
        return floors;
       }catch (e) {
           console.log(e);
       }
    }

  public async getFloorsByBuildingCodeForPassageway(buildingCode: string): Promise<IFloorDTO[]> {
     const response =  await fetch(Constants.API_FLOOR_GET_BY_BUILDING_CODE_URL + buildingCode, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
     }
    );

    if (response.status == 500) {
      alert(await response.text());
      return [];
    }
    return await response.json();
  }


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

    public async editFloor(buildingCode: string, number: number, code: string, description: string): Promise<void> {
        try{
            let hasDescription = true;

            //check if description is empty
            if (description === '' || description === null || description === undefined) {
                hasDescription = false;
            }

            const response = await this.getEditResponse(hasDescription, buildingCode, number, code, description);

            const json = await response.json();

            if (response.status === 200) {
                alert('Edited floor successfully');
                window.location.href = '/home';
            } else {
                alert(json);
            }
        }catch (e) {
            console.log(e);
        }

    }

    private async getEditResponse(hasDescription: boolean, buildingCode: string, number: number, code: string, description: string) {

        if (hasDescription) {
            return await fetch(Constants.API_FLOOR_EDIT_URL, {
                method: 'PUT',
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
            return await fetch(Constants.API_FLOOR_EDIT_URL, {
                method: 'PUT',
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