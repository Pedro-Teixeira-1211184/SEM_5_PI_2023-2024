import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {RoomId} from "./roomId";

import IRoomDTO from "../dto/IRoomDTO";

interface RoomProps {
    floorID: string;
    designation: string;
    name: string;
    localization: string;
}

export class Room extends AggregateRoot<RoomProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get RoomId(): RoomId {
    return new RoomId(this.RoomId.toValue());
  }

  get name(): string {
    return this.props.name;
  }

  set name(value: string) {
    this.props.name = value;
  }

  get localizatilon(): string {
    return this.props.localization;
  }

  set localization(value: string) {
    this.props.localization = value;
  }

  get floorID(): string {
    return this.props.floorID;
  }

  set floorID(value: string) {
    this.props.floorID = value;
  }

  get designation(): string {
        return this.props.designation;
  }

  set designation(value: string) {
        this.props.designation = value;
  }

  private constructor(props: RoomProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(RoomDTO: IRoomDTO, id?: UniqueEntityID): Result<Room> {
    const name = RoomDTO.name;
    const localizatilon = RoomDTO.localization;
    const designation = RoomDTO.designation;
    const floorID = RoomDTO.floorID;
    

    const room = new Room({
      name: name,
      localization: localizatilon,
      designation: designation,
      floorID: floorID,
    }, id);
    return Result.ok<Room>(room);
  }
}