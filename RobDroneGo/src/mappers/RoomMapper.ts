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
        id: raw.RoomID,
        floorID: raw.RoomfloorID,
        designation: raw.Roomdesignation,
        name: raw.RoomName,
        localization: raw.RoomDimensions,
      },
      new UniqueEntityID(raw.domainId)
    );


    RoomOrError.isFailure ? console.log(RoomOrError.error) : '';

    return RoomOrError.isSuccess ? RoomOrError.getValue() : null;
  }

  public static toPersistence(Room: Room): any {
    return {
      RoomID: Room.id.toString(),
      RoomfloorID: Room.floorID,
      Roomdesignation: Room.designation,
      RoomName: Room.name,
      RoomLocalization: Room.localizatilon,
    }
  }
}
