import {Inject, Service} from 'typedi';

import ISignUpRequestRepo from "../services/IRepos/ISignUpRequestRepo";
import {SignUpRequest} from "../domain/signUpRequest";
import {UserEmail} from "../domain/userEmail";
import {SignUpRequestMap} from "../mappers/SignUpRequestMap";
import {Document, FilterQuery, Model} from "mongoose";
import {ISignUpRequestPersistence} from "../dataschema/ISignUpRequestPersistence";
import {IFloorPersistence} from "../dataschema/IFloorPersistence";


@Service()
export default class SignUpRequestRepo implements ISignUpRequestRepo {

    constructor(
        @Inject('signUpRequestSchema') private signUpRequestSchema: Model<ISignUpRequestPersistence & Document>,
    ) {
    }

    public async delete(signUpRequest: SignUpRequest): Promise<boolean> {
        try {
            this.signUpRequestSchema.deleteOne({signUpRequestEmail: signUpRequest.email}, (err) => {
                if (err) return false;
            });
            return true;
        } catch (e) {
            throw e;
        }
    }

    public async exists(signUpRequest: SignUpRequest): Promise<boolean> {
        try {
            //determines if the floor exists in the database by his number and buildingCode
            const query = {signUpRequestEmail: signUpRequest.email};
            const floorDocument = await this.signUpRequestSchema.findOne(query as FilterQuery<ISignUpRequestPersistence & Document>);
            return floorDocument != null;
        } catch (error) {
            throw error;
        }
    }

    public async findByEmail(email: string): Promise<SignUpRequest> {
        try {
            //determines if the floor exists in the database by his number and buildingCode
            const query = {signUpRequestEmail: email};
            const floorDocument = await this.signUpRequestSchema.findOne(query as FilterQuery<ISignUpRequestPersistence & Document>);
            return SignUpRequestMap.toDomain(floorDocument);
        } catch (error) {
            throw error;
        }
    }

    public async save(signUpRequest: SignUpRequest): Promise<SignUpRequest> {
        try {
            //determines if the floor exists in the database by his number and buildingCode
            if (!await this.exists(signUpRequest)) {
                const pers = SignUpRequestMap.toPersistence(signUpRequest);
                const signUpRequestDocument = await this.signUpRequestSchema.create(pers);
                return SignUpRequestMap.toDomain(signUpRequestDocument);
            } else {
                return null;
            }
        } catch (e) {
            return Promise.reject(e);
        }
    }

    public async getAll(): Promise<SignUpRequest[]> {
        try {
            const signUpRequestDocuments = await this.signUpRequestSchema.find();
            let signUpRequests: SignUpRequest[] = [];
            for (let signUpRequestDocument of signUpRequestDocuments) {
                signUpRequests.push(await SignUpRequestMap.toDomain(signUpRequestDocument));
            }
            return signUpRequests;
        } catch (e) {
            return Promise.reject(e);
        }
    }

}
