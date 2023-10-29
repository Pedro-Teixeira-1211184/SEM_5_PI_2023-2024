import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";

import IFloorDTO from "../dto/IFloorDTO";
import {FloorId} from "./floorId";

interface FloorProps {
  buildingCode: string;
  number: number;
  description: string;
}

export class Floor extends AggregateRoot<FloorProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get floorId(): FloorId {
    return new FloorId(this.floorId.toValue());
  }

  get buildingCode(): string {
    return this.props.buildingCode;
  }

  set buildingCode(value: string) {
    this.props.buildingCode = value;
  }

  get number(): number {
    return this.props.number;
  }

  set number(value: number) {
    this.props.number = value;
  }

  get description(): string {
    return this.props.description;
  }

  set description(value: string) {
    this.props.description = value;
  }

  private constructor(props: FloorProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(floorDTO: IFloorDTO, id?: UniqueEntityID): Result<Floor> {
    const buildingCode = floorDTO.buildingCode;
    const number = floorDTO.number;
    const description = floorDTO.description;

    const floor = new Floor({
      buildingCode: buildingCode,
      number: number,
      description: description
    }, id);
    return Result.ok<Floor>(floor);
  }
}

