import {Inject, Service} from 'typedi';
import config from '../../config';

//import MailerService from './mailer.ts.bak';
import IUserService from '../services/IServices/IUserService';
import {UserMap} from "../mappers/UserMap";
import {IUserDTO} from '../dto/IUserDTO';

import IUserRepo from './IRepos/IUserRepo';
import IRoleRepo from './IRepos/IRoleRepo';

import {User} from '../domain/user';
import {Result} from "../core/logic/Result";
import * as crypto from "crypto";
import {ISignUpRequestDTO} from '../dto/ISignUpRequestDTO';
import {SignUpRequest} from "../domain/signUpRequest";
import {SignUpRequestMap} from "../mappers/SignUpRequestMap";
import ISignUpRequestRepo from "./IRepos/ISignUpRequestRepo";

@Service()
export default class UserService implements IUserService {
    constructor(
        @Inject(config.repos.user.name) private userRepo: IUserRepo,
        @Inject(config.repos.role.name) private roleRepo: IRoleRepo,
        @Inject(config.repos.signUpRequest.name) private signUpRequestRepo: ISignUpRequestRepo
    ) {
    }

    public async deleteUserRequest(email: string): Promise<Result<boolean>> {
        try {
            const user = await this.signUpRequestRepo.findByEmail(email);

            if (user == null) {
                return Result.fail<boolean>("User does not exist");
            }

            await this.signUpRequestRepo.delete(user);

            return Result.ok<boolean>(true);
        } catch (e) {
            throw e;
        }
    }

    //user signed up
    user: IUserDTO = null;

    public async getAllUserRequests(): Promise<Result<ISignUpRequestDTO[]>> {
        try {
            const requests = await this.signUpRequestRepo.getAll();
            if (requests == null || requests.length == 0) {
                return Result.fail<ISignUpRequestDTO[]>("No requests found");
            }
            const requestDTOs = requests.map(request => SignUpRequestMap.toDTO(request) as ISignUpRequestDTO);
            return Result.ok<ISignUpRequestDTO[]>(requestDTOs);
        } catch (e) {
            throw e;
        }
    }

    public async SignUp(dto: IUserDTO): Promise<Result<IUserDTO>> {
        try {
            const userOrError = await User.create(dto);

            if (userOrError.isFailure) {
                return Result.fail<IUserDTO>(userOrError.errorValue());
            }

            //verify if role exists
            const roleExists = await this.getRole(dto.role);

            if (roleExists.isFailure) {
                console.log('Role does not exist');
                return Result.fail<IUserDTO>(roleExists.errorValue());
            }

            const user: User = userOrError.getValue();

            //verify if email already exists
            const emailAlreadyExists = await this.userRepo.findByEmail(user.email);

            if (emailAlreadyExists != null) {
                return Result.fail<IUserDTO>("Email already exists");
            }

            await this.userRepo.save(user);

            const userDTO = UserMap.toDTO(user) as IUserDTO;

            return Result.ok<IUserDTO>(userDTO);
        } catch (e) {
            throw e;
        }
    }

    public async SignIn(email: string, password: string): Promise<Result<IUserDTO>> {
        //verify if email exists
        const user = await this.userRepo.findByEmail(email);

        if (user == null) {
            return Result.fail<IUserDTO>("Email does not exist");
        }

        const result = await this.verifyPassword(password, user.password)

        if (!result) {
            return Result.fail<IUserDTO>("Password is incorrect");
        }

        const userDTO = UserMap.toDTO(user) as IUserDTO;

        this.user = userDTO;

        return Result.ok<IUserDTO>(userDTO);
    }

    private async getRole(roleName: string): Promise<Result<string>> {

        const role = await this.roleRepo.findByName(roleName);
        const found = !!role;

        if (found) {
            return Result.ok<string>(role.name);
        } else {
            return Result.fail<string>("Couldn't find role by name=" + roleName);
        }
    }

    private async hashPassword(password: string): Promise<string> {
        return crypto.createHmac('sha256', '10').update(password).digest('hex');
    }

    private async verifyPassword(password: string, hashedPassword: string): Promise<boolean> {
        const hashedAttempt = await this.hashPassword(password);
        return hashedAttempt === hashedPassword;
    }

    public async IsSignedIn(): Promise<IUserDTO> {
        return this.user;
    }

    public async Logout(): Promise<void> {
        this.user = null;
    }

    public async deleteUser(email: string): Promise<Result<boolean>> {
        try {
            const user = await this.userRepo.findByEmail(email);

            if (user == null) {
                return Result.fail<boolean>("User does not exist");
            }
            const x = await this.userRepo.delete(user);
            return Result.ok<boolean>(x);
        } catch (e) {
            throw e;
        }
    }

    public async getUserByEmail(email: string): Promise<Result<IUserDTO>> {
        try {
            const user = await this.userRepo.findByEmail(email);

            if (user == null) {
                return Result.fail<IUserDTO>("User does not exist");
            }

            const userDTO = UserMap.toDTO(user) as IUserDTO;

            return Result.ok<IUserDTO>(userDTO);
        } catch (e) {
            throw e;
        }
    }

    public async SignUpRequest(request: ISignUpRequestDTO): Promise<Result<ISignUpRequestDTO>> {
        try {
            const requestOrError = await SignUpRequest.create(request);

            if (requestOrError.isFailure) {
                return Result.fail<ISignUpRequestDTO>(requestOrError.errorValue());
            }

            const user = requestOrError.getValue();

            //hash password
            user.password = await this.hashPassword(user.password);

            const save = await this.signUpRequestRepo.save(user);
            if (save == null) {
                return Result.fail<ISignUpRequestDTO>("Request already exists");
            }
            const requestDTO = SignUpRequestMap.toDTO(save) as ISignUpRequestDTO;
            return Result.ok<ISignUpRequestDTO>(requestDTO);
        } catch (e) {
            throw e;
        }
    }

}
