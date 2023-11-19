import { Injectable } from '@angular/core';
import IRoomDTO from "../../dto/IRoomDTO";
import IPassagewayDTO from "../../dto/IPassagewayDTO";
import IElevatorDTO from "../../dto/IElevatorDTO";

@Injectable({
  providedIn: 'root'
})
export class MapService {

  constructor() { }

  public async patchMap(buildingCode: string,floorNumber: number,length:number,width:number,map:number[][],rooms:IRoomDTO[],passageways: IPassagewayDTO[],elevators: IElevatorDTO[]): Promise<void> {


  }
}
