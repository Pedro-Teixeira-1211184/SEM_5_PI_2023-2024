import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import IElevatorDTO from "./dto/IElevatorDTO";

@Injectable({
  providedIn: 'root'
})
export class ElevatorService {

  constructor() {
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
