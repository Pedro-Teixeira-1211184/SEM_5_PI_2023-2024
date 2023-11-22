import {Injectable} from '@angular/core';
import IRoomDTO from "../../dto/IRoomDTO";
import IPassagewayDTO from "../../dto/IPassagewayDTO";
import IElevatorDTO from "../../dto/IElevatorDTO";
import Constants from "../../../utils/Constants";
import IMapRoomDTO from "../../dto/IMapRoomDTO";
import IMapPassagewayDTO from "../../dto/IMapPassagewayDTO";
import IMapElevatorDTO from "../../dto/IMapElevatorDTO";

@Injectable({
  providedIn: 'root'
})
export class MapService {

  constructor() {
  }

  public async patchMap(buildingCode: string, floorNumber: number, length: number, width: number, map: number[][], rooms: IMapRoomDTO[], passageways: IMapPassagewayDTO[], elevators: IMapElevatorDTO[]) {
    try {
      const response = await fetch(Constants.API_MAP_PATCH_URL, {
        method: 'PATCH',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          buildingCode: buildingCode,
          floorNumber: floorNumber,
          size: {
            length: length,
            width: width
          },
          map: map,
          rooms: rooms,
          passageways: passageways,
          elevator: elevators
        })
      });


      if (response.status === 200) {
        alert('Created map successfully');
        window.location.href = '/home';
      } else {
        alert(await response.text());
      }
    } catch (e) {
      console.log(e);
    }
  }

  public async loadMap(buildingCode: string, floorNumber: number) {
    try {
      const response = await fetch(Constants.API_MAP_GET_URL + '/' + buildingCode + '/' + floorNumber, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json'
        }
      });

      if (response.status === 200) {
        return await response.json();
      } else {
        alert(await response.text());
      }
    } catch (e) {
      console.log(e);
    }
  }


  public async pathBetweenFloors(origin: string, destination: string) {
    try {
      const response = await fetch(Constants.API_PATH_BETWEEN_FLOORS_URL + '/' + origin + '/' + destination, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json'
        }
      });

      if (response.status === 200) {
        return await response.json();
      } else {
        alert(await response.text());
      }
    } catch (e) {
      console.log(e);
    }
  }
}
