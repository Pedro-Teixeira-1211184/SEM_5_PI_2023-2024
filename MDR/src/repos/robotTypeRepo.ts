import {Inject, Service} from 'typedi';

import {Document, FilterQuery, Model} from "mongoose";
import IRobotTypeRepo from "../services/IRepos/IRobotTypeRepo";
import {IRobotTypePersistence} from "../dataschema/IRobotTypePersistence";
import {RobotType} from "../domain/robotType";
import {RobotTypeMapper} from "../mappers/RobotTypeMapper";
import {IRobotPersistence} from "../dataschema/IRobotPersistence";
import IRobotDTO from "../dto/IRobotDTO";
import {RobotMapper} from "../mappers/RobotMapper";
import IRobotTypeDTO from "../dto/IRobotTypeDTO";


@Service()
export default class RobotTypeRepo implements IRobotTypeRepo {

  constructor(
    @Inject('robotTypeSchema') private robotTypeSchema: Model<IRobotTypePersistence & Document>,
  ) {
  }

  public async getAll(): Promise<IRobotTypeDTO[]> {
    try {
      const query = {} as FilterQuery<IRobotPersistence & Document>;
      const robotRecord = await this.robotTypeSchema.find(query);
      const robotArray: IRobotTypeDTO[] = [];

      if (robotRecord.length === 0) {
        return [];
      } else {
        for (let i = 0; i < robotRecord.length; i++) {
          robotArray[i] = RobotTypeMapper.toDTO(RobotTypeMapper.toDomain(robotRecord[i]));
        }
        return robotArray;
      }
    } catch (e) {
      throw e;
    }
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async delete(robotType: RobotType): Promise<void> {
    return Promise.resolve(undefined);
  }

  public async exists(robotType: RobotType): Promise<boolean> {
    try {
      //determines if the floor exists in the database by his number and buildingCode
      const query = {robotTypeDesignation: robotType.designation};
      const typeDocument = await this.robotTypeSchema.findOne(query as FilterQuery<IRobotTypePersistence & Document>);
      return typeDocument == null;
    } catch (error) {
      throw error;
    }
  }

  public async findByDesignation(designation: string): Promise<RobotType> {
    try {
      const query = {robotTypeDesignation: designation};
      return this.robotTypeSchema.findOne(query as FilterQuery<IRobotTypePersistence & Document>)
        .then((type) => {
          if (type == null) return null;
          return RobotTypeMapper.toDomain(type);
        })
    } catch (error) {
      throw error;
    }
  }

  public async findByDomainId(robotTypeId: string): Promise<RobotType> {
    try {
      const query = {robotTypeId: robotTypeId};
      return this.robotTypeSchema.findOne(query as FilterQuery<IRobotTypePersistence & Document>)
        .then((type) => {
          if (type == null) return null;
          return RobotTypeMapper.toDomain(type);
        })
    } catch (error) {
      throw error;
    }
  }

  public async save(robotType: RobotType): Promise<RobotType> {
    try {
      //determines if the floor exists in the database by his number and buildingCode
      if (await this.exists(robotType)) {
        const floorDocument = await this.robotTypeSchema.create(RobotTypeMapper.toPersistence(robotType));
        return RobotTypeMapper.toDomain(floorDocument);
      } else {
        return null;
      }
    } catch (error) {
      throw error;
    }
  }

}
