import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import IPassagewayDTO from "../../dto/IPassagewayDTO";
import {from} from "rxjs";
import IFloorDTO from 'src/app/dto/IFloorDTO';

@Injectable({
  providedIn: 'root'
})
export class PassagewayService {

  constructor() {
  }

  public async createPassageway(code1: string, code2: string) {
    const response = await fetch(Constants.API_PASSAGEWAY_CREATE_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        floorCode1: code1,
        floorCode2: code2
      })
    });

    const json = await response.json();

    if (response.status == 200) {
      alert("Passageway created successfully!");
      window.location.reload();
    } else {
      alert(json);
    }
  }

  public async getPassageWayByFloorCode(floorCode: string): Promise<IPassagewayDTO[]> {
    const response = await fetch(Constants.API_PASSAGEWAY_GET_BY_FLOOR_CODE_URL + floorCode, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
    });

    if (response.status == 500) {
      alert(await response.json())
      return [];
    }

    return await response.json();
  }

  public async getPassagewaysInBuildings(buildingCode: string, buildingCode1: string): Promise<IPassagewayDTO[]> {
    const response = await fetch(Constants.API_PASSAGEWAY_GET_BETWEEN_BUILDINGS_URL + buildingCode + "/" + buildingCode1, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json'
      }
    });
    if (response.status == 500) {
      alert(await response.json())
    }
    return await response.json();
  }


  public async editPassageway(floorCode: string, floorCode1: string) {
    const response = await fetch(Constants.API_PASSAGEWAY_EDIT_URL, {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        floorCode: floorCode,
        floorCode1: floorCode1
      })
    });

    const json = await response.json();

    if (response.status == 200) {
      alert("Passageway edited successfully!");
      window.location.reload();
    } else {
      alert(json);
    }
  }
}
