import {Inject, Service} from 'typedi';

import {Document, FilterQuery, Model} from "mongoose";
import IFloorRepo from "../services/IRepos/IFloorRepo";
import {FloorMapper} from "../mappers/FloorMapper";
import {IFloorPersistence} from "../dataschema/IFloorPersistence";
import {Floor} from "../domain/floor";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";


@Service()
export default class FloorRepo implements IFloorRepo {

  constructor(
    @Inject('floorSchema') private floorSchema: Model<IFloorPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async delete(floor: Floor): Promise<void> {
    try {
      const query = {floorID: floor.id.toString()};
      await this.floorSchema.deleteOne(query as FilterQuery<IBuildingPersistence & Document>);
    } catch (error) {
      throw error;
    }
  }

  public async exists(floor: Floor): Promise<boolean> {
    try {
      //determines if the floor exists in the database by his number and buildingCode
      const query = {floorNumber: floor.number, floorBuildingCode: floor.buildingCode};
      const floorDocument = await this.floorSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      return floorDocument == null;
    } catch (error) {
      throw error;
    }
  }

  public async findByDomainId(floorId: string): Promise<Floor> {
    try {
      const query = {floorID: floorId};
      return this.floorSchema.findOne(query as FilterQuery<IFloorPersistence & Document>)
        .then((floor) => {
          if (floor == null) return null;
          return FloorMapper.toDomain(floor);
        })
    } catch (error) {
      throw error;
    }
  }

  public async save(floor: Floor): Promise<Floor> {
    try {
      //determines if the floor exists in the database by his number and buildingCode
      if (await this.exists(floor)) {
        const floorDocument = await this.floorSchema.create(FloorMapper.toPersistence(floor));
        return FloorMapper.toDomain(floorDocument);
      }else {
        return null;
      }
    } catch (error) {
      throw error;
    }
  }

}
