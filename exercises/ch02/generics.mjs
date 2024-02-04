const GENERIC_PROCS = new Map
const PROC = Symbol("proc");
const tag = (type, datum) => [Symbol(type), datum]
const typeTag = (datum) => datum[0]
const content = (datum) => datum[1]
const put = (op, type, proc) => {
  const putHelper = (types, currentPos) => {

      if(types.length === 0){
      currentPos.set(PROC, proc)
      return true
    }
    const nextType = types.shift(1)
    if (currentPos.get(nextType)) {
      return putHelper(types, currentPos.get(nextType))
    } else {
      currentPos.set(nextType, new Map([[PROC, undefined]]))
      return putHelper(types, currentPos.get(nextType) )
    }
  }
  if (GENERIC_PROCS.get(op) === undefined) {

    GENERIC_PROCS.set(op, new Map)
  }
  return putHelper(type, GENERIC_PROCS.get(op))
}
 const get = (op, types) => {
  const getProc = (typesHelper, currentPos) => {
    if (typesHelper.length === 0) {
      return currentPos.get(PROC)
    }
    currentPos = currentPos.get(typesHelper.shift())
    if (currentPos === undefined) Error(`No Procedure for operation ${op} and types ${types}`)
   return getProc(typesHelper, currentPos)
  }
  return getProc(types, GENERIC_PROCS.get(op))
}

 const applyGeneric = (op, ...args) => {
  let typeTags = args.map(typeTag)
  let proc = get(op, typeTags)
  if (proc) {
    return proc(...args.map(content))
  } else {
      Error(`No Procedure for operation ${op} and types ${typeTags}`)
  }
}

export {applyGeneric, put, get, tag}
