import {Inject, Service} from 'typedi';

import IBuildingRepo from "../services/IRepos/IBuildingRepo";
import {Building} from "../domain/building";
import {BuildingMapper} from "../mappers/BuildingMapper";
import IBuildingDTO from '../dto/IBuildingDTO';
import {BuildingId} from "../domain/buildingId";

import {Document, FilterQuery, Model} from "mongoose";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";


@Service()
export default class BuildingRepo implements IBuildingRepo {

  constructor(
    @Inject('buildingSchema') private buildingSchema: Model<IBuildingPersistence & Document>,
  ) {
  }

  public async save(building: Building): Promise<Building> {


    try {
      if (await this.exists(building)) {

        const rawBuilding: any = BuildingMapper.toPersistence(building);

        const buildingCreated = await this.buildingSchema.create(rawBuilding);

        return BuildingMapper.toDomain(buildingCreated);
      } else {
        console.log('Building already exists');
        return null;
      }
    } catch (error) {
      throw error;
    }
  }

  public async findByDomainId(buildingId: string): Promise<Building> {
    try {
      const query = {buildingID: buildingId};
      const buildingRecord = await this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      return BuildingMapper.toDomain(buildingRecord);
    } catch (error) {
      throw error;
    }
  }

  public async delete(building: Building): Promise<void> {
    try {
      const query = {buildingID: building.id.toString()};
      await this.buildingSchema.deleteOne(query as FilterQuery<IBuildingPersistence & Document>);
    } catch (error) {
      throw error;
    }
  }

  public async exists(building: Building): Promise<boolean> {
    try {
      //determines if the building exists by its unique name
      const query = {buildingName: building.name.toString()};
      const buildingDocument = await this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      return buildingDocument == null;
    } catch (error) {
      throw error;
    }
  }

  // @ts-ignore
  public async update(buildingId: string, updatedFields: Partial<IBuildingDTO>): Promise<IBuildingPersistence | null> {
    //not implemented
  }

}

