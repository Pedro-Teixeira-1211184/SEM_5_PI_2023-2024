import {Mapper} from "../core/infra/Mapper";

import {Map} from "../domain/map";
import IMapDTO from "../dto/IMapDTO";
import IMapController from "../controllers/IControllers/IMapController";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Document, Model} from "mongoose";
import {IFloorPersistence} from "../dataschema/IFloorPersistence";
import {IMapPersistence} from "../dataschema/IMapPersistence";


export class MapMapper extends Mapper<Map> {

  public static toDTO(map: Map): IMapDTO {
    return {
      id: map.id.toString(),
      buildingCode: map.buildingCode,
      floorNumber: map.floorNumber,
      size: map.size,
      map: map.map,
      rooms: map.rooms,
      passageways: map.passageways,
      elevator: map.elevator
    } as IMapDTO;
  }


  public static toDomain(raw: any | Model<IMapPersistence & Document>): Map {
    const mapOrError = Map.create(
      {
        id: raw.mapID,
        buildingCode: raw.mapBuildingCode,
        floorNumber: raw.mapFloorNumber,
        size: raw.mapSize,
        map: raw.mapMap,
        rooms: raw.mapRooms,
        passageways: raw.mapPassageways,
        elevator: raw.mapElevator
      }, new UniqueEntityID(raw.mapID)
    );

    mapOrError.isFailure ? console.log(mapOrError.error) : '';

    return mapOrError.isSuccess ? mapOrError.getValue() : null;
  }

  public static toPersistence(map: Map): any {
    return {
      mapID: map.id.toString(),
      mapBuildingCode: map.buildingCode,
      mapFloorNumber: map.floorNumber,
      mapSize: map.size,
      mapMap: map.map,
      mapRooms: map.rooms,
      mapPassageways: map.passageways,
      mapElevator: map.elevator
    };
  }

}
