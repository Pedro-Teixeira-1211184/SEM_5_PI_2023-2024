import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';
import {IRoomPersistence} from '../dataschema/IRoomPersistence';
import IRoomDTO from "../dto/IRoomDTO";
import {Room} from "../domain/room";


import {UniqueEntityID} from "../core/domain/UniqueEntityID";

export class RoomMapper extends Mapper<Room> {

  public static toDTO(Room: Room): IRoomDTO {
    return {
      id: Room.id.toString(),
      floorCode: Room.floorCode.toString(),
      designation: Room.designation.toString(),
      name: Room.name.toString(),
    } as IRoomDTO;
  }

  public static toDomain(raw: any | Model<IRoomPersistence & Document>): Room {
    const RoomOrError = Room.create(
      {
        id: raw.roomID,
        floorCode: raw.roomFloorCode,
        designation: raw.roomDesignation,
        name: raw.roomName,
      },
      new UniqueEntityID(raw.domainId)
    );


    RoomOrError.isFailure ? console.log(RoomOrError.error) : '';

    return RoomOrError.isSuccess ? RoomOrError.getValue() : null;
  }

  public static toPersistence(Room: Room): any {
    return {
      roomID: Room.id.toString(),
      roomFloorCode: Room.floorCode,
      roomDesignation: Room.designation,
      roomName: Room.name
    }
  }
}
