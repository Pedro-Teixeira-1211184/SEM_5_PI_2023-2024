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
      floorID: Room.floorID.toString(),
      designation: Room.designation.toString(),
      name: Room.name.toString(),
      localization: Room.localizatilon.toString(),
    } as IRoomDTO;
  }

  public static toDomain(raw: any | Model<IRoomPersistence & Document>): Room {
    const RoomOrError = Room.create(
      {
        id: raw.roomID,
        floorID: raw.roomFloorID,
        designation: raw.roomDesignation,
        name: raw.roomName,
        localization: raw.roomLocalization,
      },
      new UniqueEntityID(raw.domainId)
    );


    RoomOrError.isFailure ? console.log(RoomOrError.error) : '';

    return RoomOrError.isSuccess ? RoomOrError.getValue() : null;
  }

  public static toPersistence(Room: Room): any {
    return {
      roomID: Room.id.toString(),
      roomFloorID: Room.floorID,
      roomDesignation: Room.designation,
      roomName: Room.name,
      roomLocalization: Room.localizatilon,
    }
  }
}
