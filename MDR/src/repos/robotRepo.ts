import {Inject, Service} from 'typedi';

import {Document, FilterQuery, Model} from "mongoose";
import {IRobotPersistence} from "../dataschema/IRobotPersistence";
import IRobotRepo from "../services/IRepos/IRobotRepo";
import {Robot} from "../domain/robot";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";
import {RobotMapper} from "../mappers/RobotMapper";
import IRobotDTO from "../dto/IRobotDTO";



@Service()
export default class RobotRepo implements IRobotRepo {

  constructor(
    @Inject('robotSchema') private robotSchema: Model<IRobotPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  delete(robot: Robot): Promise<void> {
    return Promise.resolve(undefined);
  }

  public async exists(robot: Robot): Promise<boolean> {
    try {
      //determines if the floor exists in the database by his number and buildingCode
      const query = {robotCode: robot.code, robotNickname: robot.nickname};
      const floorDocument = await this.robotSchema.findOne(query as FilterQuery<IRobotPersistence & Document>);
      return floorDocument == null;
    } catch (error) {
      throw error;
    }
  }

  public async findByDomainId(robotId: string): Promise<Robot> {
    try {
      const query = {_id: robotId};
      return this.robotSchema.findOne(query as FilterQuery<IRobotPersistence & Document>)
        .then((document) => {
          if (document != null) {
            return RobotMapper.toDomain(document);
          }
          return null;
        });
    } catch (e) {
      throw e;
    }
  }

  public async findByNickname(nickname: string): Promise<Robot> {
    try {
      const query = {robotNickname: nickname.toString()};
      console.log(nickname);
      const floorDocument = await this.robotSchema.findOne(query as FilterQuery<IRobotPersistence & Document>);
      console.log(floorDocument);
      if (floorDocument != null) {
        return RobotMapper.toDomain(floorDocument);
      }
      return null;
    } catch (e) {
      throw e;
    }
  }

  public async save(robot: Robot): Promise<Robot> {
    try {
      //determines if the floor exists in the database by his number and buildingCode
      if (await this.exists(robot)) {
        const robotDocument = await this.robotSchema.create(RobotMapper.toPersistence(robot));
        return RobotMapper.toDomain(robotDocument);
      } else {
        return null;
      }
    } catch (error) {
      throw error;
    }
  }

  public async update(robot: Robot, id: string): Promise<Robot> {
    try {
      const query: FilterQuery<IBuildingPersistence> = {robotCode: id.toString()};
      const find = await this.robotSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      const robot1 = RobotMapper.toDomain(find);
      robot1.isActive = robot.isActive;
      const rawRobot = RobotMapper.toPersistence(robot1);
      await this.robotSchema.replaceOne(query as FilterQuery<IBuildingPersistence & Document>, rawRobot);
      return robot1;
    } catch (e) {
      throw e;
    }
  }

  public async getAll(): Promise<IRobotDTO[]> {
    try {
      const query = {} as FilterQuery<IRobotPersistence & Document>;
      const robotRecord = await this.robotSchema.find(query);
      const robotArray: IRobotDTO[] = [];

      if (robotRecord.length === 0) {
        return [];
      } else {
        for (let i = 0; i < robotRecord.length; i++) {
          robotArray[i] = RobotMapper.toDTO(RobotMapper.toDomain(robotRecord[i]));
        }
        return robotArray;
      }
    } catch (e) {
      throw e;
    }
  }

  public async findByCode(robotCode: string): Promise<Robot> {
    try {
      const query = {robotCode: robotCode};
      return this.robotSchema.findOne(query as FilterQuery<IRobotPersistence & Document>)
          .then((document) => {
            if (document != null) {
              return RobotMapper.toDomain(document);
            }
            return null;
          });
    } catch (e) {
      throw e;
    }
  }

}
