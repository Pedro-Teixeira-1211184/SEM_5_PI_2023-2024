using DDDSample1.Domain.Shared;
using System;
namespace DDDSample1.Domain.Tasks
{

    public class TaskId : EntityId
    {
        public TaskId(string value) : base(value)
        {
        }

        protected override object CreateFromString(string text)
        {
            return text; // Assuming TaskId is a string-based identifier
        }

        public override string AsString()
        {
            return (string)ObjValue; // Assuming TaskId is a string-based identifier
        }

        // Additional logic for generating a TaskId from a Guid
        public static TaskId NewTaskId()
        {
            return new TaskId(Guid.NewGuid().ToString());
        }
    }
}