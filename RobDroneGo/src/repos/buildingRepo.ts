import {Inject, Service} from 'typedi';

import IBuildingRepo from "../services/IRepos/IBuildingRepo";
import {Building} from "../domain/building";
import {BuildingMapper} from "../mappers/BuildingMapper";
import IBuildingDTO from '../dto/IBuildingDTO';

import {Document, FilterQuery, Model} from "mongoose";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";


@Service()
export default class BuildingRepo implements IBuildingRepo {

  constructor(
    @Inject('buildingSchema') private buildingSchema: Model<IBuildingPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
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


  public async update(buildingCode: string, updatedFields: Partial<IBuildingDTO>): Promise<IBuildingPersistence | null> {
    try {
      const query: FilterQuery<IBuildingPersistence> = {buildingCode: buildingCode};
      return await this.buildingSchema.findOneAndUpdate(query, updatedFields, {new: true})
    } catch (error) {
      throw error;
    }
  }

  public async getAll(): Promise<Building[]> {
    try {
      const query = {} as FilterQuery<IBuildingPersistence & Document>;
      const buildingRecord = await this.buildingSchema.find(query);
      const buildingArray: Building[] = [];

      if (buildingRecord.length === 0) {
        return [];
      } else {
        for (let i = 0; i < buildingRecord.length; i++) {
          buildingArray[i] = BuildingMapper.toDomain(buildingRecord[i]);
        }
        return buildingArray;
      }
    } catch (error) {
      console.log('Error in BuildingRepo.getAll(): ', error);
      throw error;
    }
  }
}
