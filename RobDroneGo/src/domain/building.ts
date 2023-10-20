import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {BuildingId} from "./buildingId";

import IBuildingDTO from "../dto/IBuildingDTO";

interface BuildingProps {
  name: string;
  dimensions: string;
  designation: string;
  description: string;
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

  get designation(): string {
    return this.props.designation;
  }

  set designation(value: string) {
    this.props.designation = value;
  }

  get description(): string {
    return this.props.description;
  }

  set description(value: string) {
    this.props.description = value;
  }

  private constructor(props: BuildingProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(buildingDTO: IBuildingDTO, id?: UniqueEntityID): Result<Building> {
    const name = buildingDTO.name;
    const dimensions = buildingDTO.dimensions;
    const designation = buildingDTO.designation;
    const description = buildingDTO.description;

    const building = new Building({
      name: name,
      dimensions: dimensions,
      designation: designation,
      description: description
    }, id);
    return Result.ok<Building>(building);
  }

}

