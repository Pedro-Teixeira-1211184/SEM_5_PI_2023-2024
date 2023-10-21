import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {BuildingId} from "./buildingId";

import IBuildingDTO from "../dto/IBuildingDTO";

interface BuildingProps {
  code: string;
  name: string;
  dimensions: string;
  description: string;
  maxFloors: string;
  minFloors: string;
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

  get dimensions(): string {
    return this.props.dimensions;
  }

  set dimensions(value: string) {
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

  get maxFloors(): string {
    return this.props.maxFloors;
  }

  set maxFloors(value: string) {
    this.props.maxFloors = value;
  }

  get minFloors(): string {
    return this.props.minFloors;
  }

  set minFloors(value: string) {
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

