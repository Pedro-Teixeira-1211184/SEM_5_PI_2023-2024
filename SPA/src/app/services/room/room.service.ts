import { Injectable } from '@angular/core';
import IRoomDTO from "../../dto/IRoomDTO";
import Constants from "../../../utils/Constants";

@Injectable({
  providedIn: 'root'
})
export class RoomService {

  constructor() { }

  public async getRoomsByFloorCode(floorCode: string): Promise<any> {
    try {
      const response =  await fetch(Constants.API_ROOM_GET_BY_FLOOR_CODE_URL + floorCode, {
          method: 'GET',
          headers: {
            'Content-Type': 'application/json'
          }
        }
      );

      const rooms = await response.json();

      if (response.status == 500) {
        alert(rooms);
        return [];
      }
      return rooms;
    }catch (e) {
      console.log(e);
    }
  }

  public async createRoom(floorCode: string, designation: string, name: string) {
    const response = await fetch(Constants.API_ROOM_CREATE_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        floorCode: floorCode,
        designation: designation,
        name: name
      })
      })

    const room = await response.json();

    if (response.status == 500) {
      alert("Error creating room!");
      return;
    }
    alert("Room created successfully!");
    };
}
