import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";

import IMapDTO from "../dto/IMapDTO";
import {MapId} from "./mapId";

interface MapProps {
  buildingCode: string;
  floorNumber: number;
  size: { width: number, height: number };
  map: number[][];
  rooms: { name: string, dimensions: string, door: string }[];
  passageways: { start: string, end: string, localization: string }[];
  elevator: { localization: string };
}

export class Map extends AggregateRoot<MapProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get mapID(): MapId {
    return new MapId(this.mapID.toValue());
  }

  get buildingCode(): string {
    return this.props.buildingCode;
  }

  get floorNumber(): number {
    return this.props.floorNumber;
  }

  get size(): { width: number, height: number } {
    return this.props.size;
  }

  get map(): number[][] {
    return this.props.map;
  }

  get rooms(): { name: string, dimensions: string, door: string }[] {
    return this.props.rooms;
  }

  get passageways(): { start: string, end: string, localization: string }[] {
    return this.props.passageways;
  }

  get elevator(): { localization: string } {
    return this.props.elevator;
  }

  private constructor(props: MapProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(mapDTO: IMapDTO, id?: UniqueEntityID): Result<Map> {
    const buildingCode = mapDTO.buildingCode;
    const floorNumber = mapDTO.floorNumber;
    const size = mapDTO.size;
    const map1 = mapDTO.map;
    const rooms = mapDTO.rooms;
    const passageways = mapDTO.passageways;
    const elevator = mapDTO.elevator;

    const map = new Map({
      buildingCode: buildingCode,
      floorNumber: floorNumber,
      size: size,
      map: map1,
      rooms: rooms,
      passageways: passageways,
      elevator: elevator
    }, id);
    return Result.ok<Map>(map);
  }

}

