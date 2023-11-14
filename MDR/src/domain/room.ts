import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {RoomId} from "./roomId";

import IRoomDTO from "../dto/IRoomDTO";

interface RoomProps {
    floorCode: string;
    designation: string;
    name: string;
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

  get floorCode(): string {
    return this.props.floorCode;
  }

  set floorCode(value: string) {
    this.props.floorCode = value;
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
    const designation = RoomDTO.designation;
    const floorCode = RoomDTO.floorCode;


    const room = new Room({
      name: name,
      floorCode: floorCode,
      designation: designation
    }, id);
    return Result.ok<Room>(room);
  }
}
