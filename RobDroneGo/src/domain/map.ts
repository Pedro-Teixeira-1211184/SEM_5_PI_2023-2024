import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";

import IMapDTO from "../dto/IMapDTO";
import {MapId} from "./mapId";

interface MapProps {
  floorID: string;
}

export class Map extends AggregateRoot<MapProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get mapID(): MapId {
    return new MapId(this.mapID.toValue());
  }

  get floorID(): string {
    return this.props.floorID;
  }

  private constructor(props: MapProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(mapDTO: IMapDTO, id?: UniqueEntityID): Result<Map> {
    const floorID = mapDTO.floorID;

    const map = new Map({
      floorID: floorID,
    }, id);
    return Result.ok<Map>(map);
  }

}

