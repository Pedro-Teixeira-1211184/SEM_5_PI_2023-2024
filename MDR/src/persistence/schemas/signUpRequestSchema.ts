import mongoose from 'mongoose';
import {ISignUpRequestPersistence} from "../../dataschema/ISignUpRequestPersistence";

const SignUpRequest = new mongoose.Schema(
    {
        domainId: {type: String, unique: true},

        firstName: {type: String, required: true},

        lastName: {type: String, required: true},

        email: {type: String, lowercase: true, unique: true},

        password: {type: String, required: true}
    },
    {timestamps: true},
);

export default mongoose.model<ISignUpRequestPersistence & mongoose.Document>('SignUpRequest', SignUpRequest);
