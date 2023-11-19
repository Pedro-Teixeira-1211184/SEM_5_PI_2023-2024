import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import IElevatorDTO from "../../dto/IElevatorDTO";

@Injectable({
  providedIn: 'root'
})
export class ElevatorService {

  constructor() {
  }

  public async createElevator(coordenates: string, buildingCode: string, floorNumbers: string): Promise<void> {
    try{
      const response = await this.getResponse(coordenates, buildingCode, floorNumbers);

      const json = await response.json();

      if(response.status === 200){
        alert('Created elevator successfully');
        window.location.href = '/home';
      }else{
        alert(json);
      }
    }catch (e) {
      console.log(e);
    }
  }

  private async getResponse(coordenates: string, buildingCode: string, floorNumbers: string) {
    return await fetch(Constants.API_ELEVATOR_CREATE_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        coordenates: coordenates,
        buildingCode: buildingCode,
        floorNumbers: floorNumbers
      })
    });
  }

  public async getElevatorsByBuildingCode(buildingCode: string): Promise<IElevatorDTO[]> {
    const response = await fetch(Constants.API_BUILDING_GET_ALL_URL + '/' + buildingCode + '/' + Constants.API_ELEVATOR_GET_BY_BUILDING_CODE_URL, {
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
}
