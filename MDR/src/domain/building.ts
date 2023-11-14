import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {BuildingId} from "./buildingId";

import IBuildingDTO from "../dto/IBuildingDTO";

interface BuildingProps {
  code: string;
  name: string;
  dimensions: { length: number, width: number };
  description: string;
  maxFloors: number;
  minFloors: number;
}

export class Building extends AggregateRoot<BuildingProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get buildingId(): BuildingId {
    return new BuildingId(this.buildingId.toValue());
  }

  get name(): string {
    return this.props.name;
  }

  set name(value: string) {
    this.props.name = value;
  }

  get dimensions(): { length: number, width: number } {
    return this.props.dimensions;
  }

  set dimensions(value: { length: number, width: number }) {
    this.props.dimensions = value;
  }

  get code(): string {
    return this.props.code;
  }

  set code(value: string) {
    this.props.code = value;
  }

  get description(): string {
    return this.props.description;
  }

  set description(value: string) {
    this.props.description = value;
  }

  get maxFloors(): number {
    return this.props.maxFloors;
  }

  set maxFloors(value: number) {
    this.props.maxFloors = value;
  }

  get minFloors(): number {
    return this.props.minFloors;
  }

  set minFloors(value: number) {
    this.props.minFloors = value;
  }

  private constructor(props: BuildingProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(buildingDTO: IBuildingDTO, id?: UniqueEntityID): Result<Building> {
    const name = buildingDTO.name;
    const dimensions = buildingDTO.dimensions;
    const designation = buildingDTO.code;
    const description = buildingDTO.description;
    const maxFloors = buildingDTO.maxFloors;
    const minFloors = buildingDTO.minFloors;

    const building = new Building({
      name: name,
      dimensions: dimensions,
      code: designation,
      description: description,
      maxFloors: maxFloors,
      minFloors: minFloors
    }, id);
    return Result.ok<Building>(building);
  }

}

