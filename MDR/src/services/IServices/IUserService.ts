import {Result} from "../../core/logic/Result";
import {IUserDTO} from "../../dto/IUserDTO";
import {ISignUpRequestDTO} from "../../dto/ISignUpRequestDTO";

export default interface IUserService {
    SignUp(userDTO: IUserDTO): Promise<Result<IUserDTO>>;

    SignIn(email: string, password: string): Promise<Result<IUserDTO>>;

    IsSignedIn(): Promise<IUserDTO>;

    Logout(): Promise<void>;

    deleteUser(email: string): Promise<Result<boolean>>;

    getUserByEmail(email: string): Promise<Result<IUserDTO>>;

    SignUpRequest(request: ISignUpRequestDTO): Promise<Result<ISignUpRequestDTO>>;
}
