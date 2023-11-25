import {Inject, Service} from 'typedi';

import IElevatorRepo from "../services/IRepos/IElevatorRepo";
import {Elevator} from "../domain/elevator";
import {ElevatorMapper} from "../mappers/ElevatorMapper";
import IElevatorDTO from '../dto/IElevatorDTO';

import {Document, FilterQuery, Model} from "mongoose";
import {IElevatorPersistence} from "../dataschema/IElevatorPersistence";


@Service()
export default class ElevatorRepo implements IElevatorRepo {

  constructor(
    @Inject('elevatorSchema') private elevatorSchema: Model<IElevatorPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }


  public async exists(elevator: Elevator): Promise<boolean> { //TODO: check if exists by buildingCode and floorNumbers
    try {
      const query = {elevatorBuildingCode: elevator.buildingCode};
      const elevatorRecord = await this.elevatorSchema.findOne(query as FilterQuery<IElevatorPersistence & Document>);
      return elevatorRecord == null;
    } catch (error) {
      throw error;
    }
  }

  public async save(elevator: Elevator): Promise<Elevator> {

    try {
      if (await this.exists(elevator)) {
        const rawElevator: any = ElevatorMapper.toPersistence(elevator);

        const elevatorCreated = await this.elevatorSchema.create(rawElevator);

        return ElevatorMapper.toDomain(elevatorCreated);
      } else {
        console.log('Door already exists');
        return null;
      }
    } catch (error) {
      throw error;
    }
  }

  public async delete(elevator: Elevator): Promise<void> {
    try {
      const query = {buildingCode: elevator.buildingCode.toString(), floorNumbers: elevator.floorNumbers.toString()};
      await this.elevatorSchema.deleteOne(query as FilterQuery<IElevatorPersistence & Document>);
    } catch (error) {
      throw error;
    }
  }


  public async findByDomainId(elevatorId: string): Promise<Elevator> {
    try {
      const query = {elevatorID: elevatorId};
      const elevatorRecord = await this.elevatorSchema.findOne(query as FilterQuery<IElevatorPersistence & Document>);
      return ElevatorMapper.toDomain(elevatorRecord);
    } catch (error) {
      throw error;
    }
  }

  public async findByBuildingCode(buildingCode: string): Promise<Elevator> {
    try {
      const query = {buildingCode: buildingCode};
      const elevatorRecord = await this.elevatorSchema.findOne(query as FilterQuery<IElevatorPersistence & Document>);
      if (elevatorRecord == null) {
        return null;
      }
      return ElevatorMapper.toDomain(elevatorRecord);
    } catch (error) {
      throw error;
    }
  }

  public async getElevatorsByBuildingCode(buildingCode: string): Promise<Elevator[]> {
    try {
      const query = {elevatorBuildingCode: buildingCode};
      const elevatorRecords = await this.elevatorSchema.find(query as FilterQuery<IElevatorPersistence & Document>);
      if (elevatorRecords.length == 0) {
        return null;
      }
      const elevators: Elevator[] = [];
      for (let i = 0; i < elevatorRecords.length; i++) {
        elevators.push(ElevatorMapper.toDomain(elevatorRecords[i]));
      }
      return elevators;
    }catch (e) {
      throw e;
    }
  }

  public async getAll(): Promise<Elevator[]> {
    try {
      const elevatorRecords = await this.elevatorSchema.find();
      if (elevatorRecords.length == 0) {
        return null;
      }
      const elevators: Elevator[] = [];
      for (let i = 0; i < elevatorRecords.length; i++) {
        elevators.push(ElevatorMapper.toDomain(elevatorRecords[i]));
      }
      return elevators;
    }catch (e) {
      throw e;
    }
  }
}
