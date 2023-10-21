import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {BuildingId} from "./buildingId";

import IFloorDTO from "../dto/IFloorDTO";

interface FloorProps {
  buildingCode: string;
  number: string;
  description: string;
}

export class Floor extends AggregateRoot<FloorProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get floorId(): BuildingId {
    return new BuildingId(this.floorId.toValue());
  }

  get buildingCode(): string {
    return this.props.buildingCode;
  }

  set buildingCode(value: string) {
    this.props.buildingCode = value;
  }

  get number(): string {
    return this.props.number;
  }

  set number(value: string) {
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

