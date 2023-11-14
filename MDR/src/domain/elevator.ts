import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {ElevatorId} from "./elevatorId";

import IElevatorDTO from "../dto/IElevatorDTO";

interface ElevatorProps {
  coordenates: string;
  buildingCode: string;
  floorNumbers: string;     // 1,2,3,4,5,6,7,8,9,10 -> divided by comma
}

export class Elevator extends AggregateRoot<ElevatorProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get elevatorId(): ElevatorId {
    return new ElevatorId(this.elevatorId.toValue());
  }

  get coordenates(): string {
    return this.props.coordenates;
  }

  set coordenates(value: string) {
    this.props.coordenates = value;
  }

  get buildingCode(): string {
    return this.props.buildingCode;
  }

  set buildingCode(value: string) {
    this.props.buildingCode = value;
  }

  get floorNumbers(): string {
    return this.props.floorNumbers;
  }

  set floorNumbers(value: string) {
    this.props.floorNumbers = value;
  }

  private constructor(props: ElevatorProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(elevatorDTO: IElevatorDTO, id?: UniqueEntityID): Result<Elevator> {
    const coordenates = elevatorDTO.coordenates;
    const buildingCode = elevatorDTO.buildingCode;
    const floorNumbers = elevatorDTO.floorNumbers;

    const elevator = new Elevator({
      coordenates: coordenates,
      buildingCode: buildingCode,
      floorNumbers: floorNumbers
    }, id);

    return Result.ok<Elevator>(elevator);
  }
}
