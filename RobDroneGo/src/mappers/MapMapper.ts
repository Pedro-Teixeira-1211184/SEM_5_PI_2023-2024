import {Mapper} from "../core/infra/Mapper";

import {Map} from "../domain/map";
import IMapDTO from "../dto/IMapDTO";
import IMapController from "../controllers/IControllers/IMapController";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";


export class MapMapper extends Mapper<Map> {

  public static toDTO(map: Map): IMapDTO {
    return {
      id: map.id.toString(),
      floorID: map.floorID,
    } as IMapDTO;
  }


  public static toDomain(raw: any): Map {
    const mapOrError = Map.create(
      {
        id: raw._id.toString(),
        floorID: raw.mapFloorID
      }, new UniqueEntityID(raw.mapID)
    );

    mapOrError.isFailure ? console.log(mapOrError.error) : '';

    return mapOrError.isSuccess ? mapOrError.getValue() : null;
  }

  public static toPersistence(map: Map): any {
    return {
      mapID: map.id.toString(),
      mapFloorID: map.floorID
    };
  }

}
