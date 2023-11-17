import {Injectable} from '@angular/core';
import Constants from "../../../utils/Constants";
import IPassagewayDTO from "../../component/passageway/dto/IPassagewayDTO";
import {from} from "rxjs";

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
    }

    return await response.json();
  }

}
